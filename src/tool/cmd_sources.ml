(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let exts_of_doms = function
| [] -> Ok None
| doms ->
    let add_domain acc d =
      let d = Result.error_to_failure (Brzo_domain.find d Brzo_domain_list.v) in
      String.Set.union (Brzo_domain.fingerprint d) acc
    in
    try Ok (Some (List.fold_left add_domain String.Set.empty doms)) with
    | Failure e -> Error e

let sources ~conf ~doms =
  Log.if_error ~use:Brzo.Exit.undefined_domain @@
  let* exts = exts_of_doms doms in
  Log.if_error' ~use:Brzo.Exit.some_error @@
  let* pager = B0_pager.find ~don't:(Brzo.Conf.no_pager conf) () in
  let* () = B0_pager.page_stdout pager in
  let* src_by_exts = Brzo.Conf.srcs conf in
  let srcs =
    let add_files ext srcs acc = match exts with
    | None -> List.rev_append srcs acc
    | Some exts when String.Set.mem ext exts -> List.rev_append srcs acc
    | Some _ -> acc
    in
    String.Map.fold add_files src_by_exts []
  in
  match List.sort Fpath.compare srcs with
  | [] -> Ok Brzo.Exit.ok
  | srcs ->
      Log.stdout (fun m -> m "@[<v>%a@]" (Fmt.list Fpath.pp_unquoted) srcs);
      Ok Brzo.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Show source files" in
  let exits = Brzo.Exit.Info.undefined_domain :: Brzo.Exit.Info.base_cmd in
  let envs = B0_pager.Env.infos in
  let man = [
    `S Manpage.s_description;
    `P "The $(cmd) command lists the source files considered by a $(tool) \
        invocation.";
    Brzo.Cli.man_see_manual; ]
  in
  Cmd.v (Cmd.info "sources" ~doc ~exits ~envs ~man) @@
  let+ conf = Brzo_tie_conf.use_brzo_file
  and+ doms =
    let doc =
      "Output files considered for selecting domain $(docv). Repeatable."
    in
    Arg.(value & opt_all string [] & info ["d"; "domain"] ~doc ~docv:"DOMAIN")
  in
  sources ~conf ~doms
