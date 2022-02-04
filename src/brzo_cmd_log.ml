(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open Result.Syntax

let log c format details op_selector =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let don't = Brzo.Conf.no_pager c || format = `Trace_event in
  let log_file = Brzo.Conf.log_file c in
  let* pager = B00_pager.find ~don't () in
  let* () = B00_pager.page_stdout pager in
  let* l = B00_cli.Memo.Log.read log_file in
  B00_cli.Memo.Log.out Fmt.stdout format details op_selector ~path:log_file l;
  Ok Brzo.Exit.ok

(* Command line interface *)

open Cmdliner

let cmd =
  let doc = "Show build log" in
  let docs_format = "OUTPUT FORMATS" in
  let docs_details = "OUTPUT DETAILS" in
  let docs_select = "OPTIONS FOR SELECTING OPERATIONS" in
  let exits = Brzo.Exit.Info.base_cmd in
  let envs = B00_pager.envs () in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) command shows build information and operations in \
      various formats.";
    `S docs_format;
    `S docs_details;
    `P "If applicable.";
    `S docs_select;
    `Blocks B00_cli.Op.query_man;
    Brzo.Cli.man_see_manual; ]
  in
  Cmd.v (Cmd.info "log" ~doc ~exits ~envs ~man)
    Term.(const log $ Brzo_tie_conf.auto_cwd_root_and_no_brzo_file $
          B00_cli.Memo.Log.out_format_cli ~docs:docs_format () $
          B00_cli.Arg.output_details ~docs:docs_details () $
          B00_cli.Op.query_cli ~docs:docs_select ())

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
