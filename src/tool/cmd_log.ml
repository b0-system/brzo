(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let log ~conf ~format ~output_details ~query =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let no_pager = Brzo.Conf.no_pager conf || format = `Trace_event in
  let log_file = Brzo.Conf.log_file conf in
  let* pager = B0_pager.find ~no_pager () in
  let* () = B0_pager.page_stdout pager in
  let* log = B0_memo_log.read log_file in
  let pp =
    B0_memo_cli.Log.pp ~format ~output_details ~query ~path:log_file ()
  in
  Fmt.pr "@[<v>%a@]@?" pp log;
  Ok Brzo.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Output build log" in
  let exits = Brzo.Exit.Info.base_cmd in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) outputs build information and operations in various formats.";
    `S B0_std_cli.s_output_details_options;
    `S B0_memo_cli.Op.s_selection_options;
    `Blocks B0_memo_cli.Op.query_man;
    Brzo.Cli.man_see_manual; ]
  in
  Cmd.make (Cmd.info "log" ~doc ~exits ~man) @@
  let+ conf = Brzo_tie_conf.auto_cwd_root_and_no_brzo_file
  and+ format = B0_memo_cli.Log.format_cli ()
  and+ output_details = B0_std_cli.output_details ()
  and+ query = B0_memo_cli.Op.query_cli () in
  log ~conf ~format ~output_details ~query
