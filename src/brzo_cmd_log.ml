(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let log c format details op_selector =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let don't = Brzo.Conf.no_pager c || format = `Trace_event in
  let log_file = Brzo.Conf.log_file c in
  let* pager = B0_pager.find ~don't () in
  let* () = B0_pager.page_stdout pager in
  let* l = B0_memo_log.read log_file in
  B0_cli.Memo.Log.out Fmt.stdout format details op_selector ~path:log_file l;
  Ok Brzo.Exit.ok

(* Command line interface *)

open Cmdliner

let cmd =
  let doc = "Show build log" in
  let exits = Brzo.Exit.Info.base_cmd in
  let envs = B0_pager.Env.infos in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) command shows build information and operations in \
        various formats.";
    `S B0_std_cli.s_output_format_options;
    `S B0_cli.Op.s_selection_options;
    `Blocks B0_cli.Op.query_man;
    Brzo.Cli.man_see_manual; ]
  in
  Cmd.v (Cmd.info "log" ~doc ~exits ~envs ~man)
    Term.(const log $ Brzo_tie_conf.auto_cwd_root_and_no_brzo_file $
          B0_cli.Memo.Log.format_cli () $ B0_std_cli.output_format () $
          B0_cli.Op.query_cli ())
