(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

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

let build_dir (type c) (module D : T with type Conf.t = c) o m c dc k =
  Brzo_outcome.build_dir_suff o m c dc @@ fun suff ->
  let d = Fmt.str "%s-%s%s" D.name (Brzo_outcome.name o) suff in
  let d = Fpath.(Brzo.Conf.b0_dir c / Brzo.Conf.brzo_dir_name / d) in
  k d

let run_outcome_build o m c dc ~build_dir ~artefact k =
  let m = B00.Memo.with_mark m (Fmt.str "%s-build" (Brzo_outcome.name o)) in
  let srcs = B00.Memo.fail_if_error m (Brzo.Conf.srcs c) in
  B00.Memo.delete m build_dir @@ fun () ->
  B00.Memo.mkdir m build_dir @@ fun _ ->
  (Brzo_outcome.build o) m c dc ~build_dir ~artefact ~srcs k

let run_outcome_action o m c dc ~build_dir ~artefact k =
  let m = B00.Memo.with_mark m (Fmt.str "%s-action" (Brzo_outcome.name o)) in
  match Os.Path.exists artefact |> Log.if_error ~use:false with
  | false ->
      Log.err (fun m -> m "No outcome, did you build before ?");
      k (`Exit Brzo.Exit.no_build_outcome)
  | true ->
      if not (Brzo_outcome.action_has_args o) && Brzo.Conf.action_args c <> []
      then warn_if_action_args ~cause:"action has no arguments" c;
      (Brzo_outcome.action o) m c dc ~build_dir ~artefact @@ fun act ->
      k (`Exec act)

let action_mode c domain m k =
  let o, dc = get_outcome_and_conf c domain in
  build_dir domain o m c dc @@ fun build_dir ->
  (Brzo_outcome.artefact o) m c dc ~build_dir @@ fun artefact ->
  run_outcome_action o m c dc ~build_dir ~artefact k

let conf_mode (type c) c (module D : T with type Conf.t = c) =
  Log.if_error' ~use:Brzo.Exit.some_error @@
  let outcome_name, dc = Option.get (Brzo.Conf.domain_conf c (module D)) in
  Result.bind (B00_pager.find ~don't:(Brzo.Conf.no_pager c) ()) @@ fun pager ->
  Result.bind (B00_pager.page_stdout pager) @@ fun () ->
  Log.app begin fun m ->
    m "@[<v>%a domain@,%a@,%a@,@,%a@]"
      Fmt.(code string) D.doc_name
      (Fmt.field "outcome" Fmt.id Fmt.string) outcome_name
      D.Conf.pp dc Brzo.Conf.pp_show c
  end;
  Ok Brzo.Exit.ok

let path_mode c domain m k =
  let o, dc = get_outcome_and_conf c domain in
  build_dir domain o m c dc @@ fun build_dir ->
  (Brzo_outcome.artefact o) m c dc ~build_dir @@ fun artefact ->
  Log.app (fun m -> m "%a" Fpath.pp_unquoted artefact);
  k (`Exit Brzo.Exit.ok)

let delete_mode c domain m k =
  let o, dc = get_outcome_and_conf c domain in
  build_dir domain o m c dc @@ fun build_dir ->
  B00.Memo.delete m build_dir @@ fun () ->
  k (`Exit Brzo.Exit.ok)

let build_mode c domain m k =
  let o, dc = get_outcome_and_conf c domain in
  build_dir domain o m c dc @@ fun build_dir ->
  (Brzo_outcome.artefact o) m c dc ~build_dir @@ fun artefact ->
  run_outcome_build o m c dc ~build_dir ~artefact @@ fun () ->
  k (`Exit Brzo.Exit.ok)

let normal_mode c domain m k =
  let o, dc = get_outcome_and_conf c domain in
  build_dir domain o m c dc @@ fun build_dir ->
  (Brzo_outcome.artefact o) m c dc ~build_dir @@ fun artefact ->
  run_outcome_build o m c dc ~build_dir ~artefact @@ fun () ->
  B00.Memo.stir m ~block:true;
  if B00.Memo.has_failures m
  then k (`Exit Brzo.Exit.outcome_build_error)
  else run_outcome_action o m c dc ~build_dir ~artefact k

let run_memo ~with_log c f =
  Result.bind (Brzo.Conf.memo c) @@ fun m ->
  let m = B00.Memo.with_mark m "setup" in
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
      let hint = Fmt.must_be and pp_domain = Fmt.(code string) in
      let unknown = Fmt.(unknown' ~kind:(any "domain") pp_domain ~hint) in
      Fmt.error "@[%a@]" unknown (n, List.map name ds)

let of_conf c ds = match Brzo.Conf.domain_name c with
| Some d -> find d ds
| None ->
    Result.bind (Brzo.Conf.srcs c) @@ fun srcs ->
    let has_srcs d = B00_fexts.exists_file (fingerprint d) srcs in
    try Ok (List.find has_srcs ds) with
    | Not_found ->
        Fmt.error
          "No sources found and no domain set in %a file.@,Maybe check %a."
          Fmt.(code string) Brzo.Conf.brzo_file_name
          Fmt.(code string) "brzo --conf"

(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
