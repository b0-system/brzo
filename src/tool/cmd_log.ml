(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let log ~conf ~log_format ~output_details ~op_query =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let don't = Brzo.Conf.no_pager conf || log_format = `Trace_event in
  let log_file = Brzo.Conf.log_file conf in
  let* pager = B0_pager.find ~don't () in
  let* () = B0_pager.page_stdout pager in
  let* l = B0_memo_log.read log_file in
  B0_cli.Memo.Log.out
    Fmt.stdout log_format output_details op_query ~path:log_file l;
  Ok Brzo.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Show build log" in
  let exits = Brzo.Exit.Info.base_cmd in
  let envs = B0_pager.Env.infos in
  let man = [
    `S Manpage.s_description;
    `P "The $(cmd) command shows build information and operations in \
        various formats.";
    `S B0_std_cli.s_output_details_options;
    `S B0_cli.Op.s_selection_options;
    `Blocks B0_cli.Op.query_man;
    Brzo.Cli.man_see_manual; ]
  in
  Cmd.make (Cmd.info "log" ~doc ~exits ~envs ~man) @@
  let+ conf = Brzo_tie_conf.auto_cwd_root_and_no_brzo_file
  and+ log_format = B0_cli.Memo.Log.format_cli ()
  and+ output_details = B0_std_cli.output_details ()
  and+ op_query = B0_cli.Op.query_cli () in
  log ~conf ~log_format ~output_details ~op_query
