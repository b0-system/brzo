(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

let log c format details op_selector =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let don't = Brzo.Conf.no_pager c || format = `Trace_event in
  let log_file = Brzo.Conf.log_file c in
  Result.bind (B00_pager.find ~don't ()) @@ fun pager ->
  Result.bind (B00_pager.page_stdout pager) @@ fun () ->
  Result.bind (B00_ui.Memo.Log.read log_file) @@ fun l ->
  B00_ui.Memo.Log.out Fmt.stdout format details op_selector ~path:log_file l;
  Ok Brzo.Exit.ok

(* Command line interface *)

open Cmdliner

let doc = "Show build log"
let sdocs = Manpage.s_common_options
let docs_format = "OUTPUT FORMATS"
let docs_details = "OUTPUT DETAILS"
let docs_select = "OPTIONS FOR SELECTING OPERATIONS"
let exits = Brzo.Exit.Info.base_cmd
let envs = B00_pager.envs ()
let man_xrefs = [ `Main ]
let man = [
  `S Manpage.s_description;
  `P "The $(tname) command shows build information and operations in
      various formats.";
  `S docs_format;
  `S docs_details;
  `P "If applicable.";
  `S docs_select;
  `Blocks B00_ui.Op.query_man;
  Brzo.Cli.man_see_manual; ]

let cmd =
  Term.(const log $ Brzo_tie_conf.auto_cwd_root_and_no_brzo_file $
        B00_ui.Memo.Log.out_format_cli ~docs:docs_format () $
        B00_ui.Cli.out_details ~docs:docs_details () $
        B00_ui.Op.query_cli ~docs:docs_select ()),
  Term.info "log" ~doc ~sdocs ~exits ~envs ~man ~man_xrefs

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
