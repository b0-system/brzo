(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_std.Fut.Syntax

(* Domains *)

module type T = sig
  include Brzo.Pre_domain.T
  val outcomes : Conf.t Brzo_outcome.t list
end

type t = V : (module T with type Conf.t = 'a) -> t

let name (V (module D)) = D.name
let fingerprint (V (module D)) = D.fingerprint
let pre_domain (V (module D)) = Brzo.Pre_domain.V (module D)

(* Running domains *)

let warn_if_action_args ~cause c =
  if Brzo.Conf.action_args c = [] then () else
  Log.warn (fun m -> m "Ignoring action arguments, %s." cause)

let get_outcome_and_conf (type c) c (module D : T with type Conf.t = c) =
  let outcome_name, dc = Option.get (Brzo.Conf.domain_conf c (module D)) in
  Brzo_outcome.get outcome_name D.outcomes, dc

let build_dir (type c) (module D : T with type Conf.t = c) o m c dc =
  let* suff = Brzo_outcome.build_dir_suff o m c dc in
  let d = Fmt.str "%s-%s%s" D.name (Brzo_outcome.name o) suff in
  let d = Fpath.(Brzo.Conf.b0_dir c / Brzo.Conf.brzo_dir_name / d) in
  Fut.return d

let run_outcome_build o m c dc ~build_dir ~artefact =
  let m = B0_memo.with_mark m (Fmt.str "%s-build" (Brzo_outcome.name o)) in
  let srcs = B0_memo.fail_if_error m (Brzo.Conf.srcs c) in
  let* () = B0_memo.delete m build_dir in
  let* () = B0_memo.mkdir m build_dir in
  (Brzo_outcome.build o) m c dc ~build_dir ~artefact ~srcs

let run_outcome_action o m c dc ~build_dir ~artefact =
  let m = B0_memo.with_mark m (Fmt.str "%s-action" (Brzo_outcome.name o)) in
  match Os.Path.exists artefact |> Log.if_error ~use:false with
  | false ->
      Log.err (fun m -> m "No outcome, did you build before ?");
      Fut.return (`Exit Brzo.Exit.no_build_outcome)
  | true ->
      if not (Brzo_outcome.action_has_args o) && Brzo.Conf.action_args c <> []
      then warn_if_action_args ~cause:"action has no arguments" c;
      let* act = (Brzo_outcome.action o) m c dc ~build_dir ~artefact in
      Fut.return (`Exec act)

let action_mode c domain m () =
  let o, dc = get_outcome_and_conf c domain in
  let* build_dir = build_dir domain o m c dc in
  let* artefact = (Brzo_outcome.artefact o) m c dc ~build_dir in
  run_outcome_action o m c dc ~build_dir ~artefact

let conf_mode (type c) c (module D : T with type Conf.t = c) =
  Log.if_error' ~use:Brzo.Exit.some_error @@
  let outcome_name, dc = Option.get (Brzo.Conf.domain_conf c (module D)) in
  Result.bind (B0_pager.find ~don't:(Brzo.Conf.no_pager c) ()) @@ fun pager ->
  Result.bind (B0_pager.page_stdout pager) @@ fun () ->
  Log.stdout begin fun m ->
    m "@[<v>%a domain@,%a@,%a@,@,%a@]"
      Fmt.code D.doc_name
      (Fmt.field "outcome" Fun.id Fmt.string) outcome_name
      D.Conf.pp dc Brzo.Conf.pp_show c
  end;
  Ok Brzo.Exit.ok

let path_mode c domain m () =
  let o, dc = get_outcome_and_conf c domain in
  let* build_dir = build_dir domain o m c dc in
  let* artefact = (Brzo_outcome.artefact o) m c dc ~build_dir in
  Log.stdout (fun m -> m "%a" Fpath.pp_unquoted artefact);
  Fut.return (`Exit Brzo.Exit.ok)

let delete_mode c domain m () =
  let o, dc = get_outcome_and_conf c domain in
  let* build_dir = build_dir domain o m c dc in
  let* () = B0_memo.delete m build_dir in
  Fut.return (`Exit Brzo.Exit.ok)

let build_mode c domain m () =
  let o, dc = get_outcome_and_conf c domain in
  let* build_dir = build_dir domain o m c dc in
  let* artefact = (Brzo_outcome.artefact o) m c dc ~build_dir in
  let* () = run_outcome_build o m c dc ~build_dir ~artefact in
  if Brzo.Conf.output_outcome_path c
  then Log.stdout (fun m -> m "%a" Fpath.pp_unquoted artefact);
  Fut.return (`Exit Brzo.Exit.ok)

let normal_mode c domain m () =
  let o, dc = get_outcome_and_conf c domain in
  let* build_dir = build_dir domain o m c dc in
  let* artefact = (Brzo_outcome.artefact o) m c dc ~build_dir in
  let* () = run_outcome_build o m c dc ~build_dir ~artefact in
  B0_memo.stir m ~block:true;
  if B0_memo.has_failures m
  then Fut.return (`Exit Brzo.Exit.outcome_build_error)
  else run_outcome_action o m c dc ~build_dir ~artefact

let run_memo ~with_log c f =
  Result.bind (Brzo.Conf.memo c) @@ fun m ->
  let m = B0_memo.with_mark m "setup" in
  match Brzo.Memo.run ~with_log m (f m) with
  | Ok (`Exit r) -> Ok r
  | Ok (`Exec a) -> Log.if_error' ~use:Brzo.Exit.outcome_action_error @@ a ()
  | Error () -> Ok Brzo.Exit.outcome_build_error

let run c (V domain) =
  Log.if_error ~use:Brzo.Exit.conf_error @@
  let log_file = Some (Brzo.Conf.log_file c) in
  match Brzo.Conf.outcome_mode c with
  | `Action -> run_memo ~with_log:log_file c (action_mode c domain)
  | `Build ->
      warn_if_action_args ~cause:"build only mode" c;
      run_memo ~with_log:log_file c (build_mode c domain)
  | `Conf -> conf_mode c domain
  | `Delete -> run_memo ~with_log:None c (delete_mode c domain)
  | `Normal -> run_memo ~with_log:log_file c (normal_mode c domain)
  | `Path -> run_memo ~with_log:None c (path_mode c domain)

(* Finding *)

let find n ds =
  let has_name id (V (module D)) = String.equal id D.name in
  try Ok (List.find (has_name n) ds) with
  | Not_found ->
      let hint = Fmt.must_be and pp_domain = Fmt.code in
      let unknown = Fmt.(unknown' ~kind:(any "domain") pp_domain ~hint) in
      Fmt.error "@[%a@]" unknown (n, List.map name ds)

let of_conf c ds = match Brzo.Conf.domain_name c with
| Some d -> find d ds
| None ->
    Result.bind (Brzo.Conf.srcs c) @@ fun srcs ->
    let has_srcs d = B0_file_exts.exists_file (fingerprint d) srcs in
    try Ok (List.find has_srcs ds) with
    | Not_found ->
        Fmt.error
          "No sources found and no domain set in %a file.@,Maybe check %a."
          Fmt.code Brzo.Conf.brzo_file_name
          Fmt.code "brzo --conf"
