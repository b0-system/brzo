(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Fut.Syntax
open B00

module Conf = struct
  type t = unit
  let tid = Tid.create ()
  let conf_get _ = Ok ()
  let keys = String.Set.empty
  let parse = conf_get
  let parse_with_cli = Cmdliner.Term.const conf_get
  let pp = Fmt.nop
end

let name = "cmark"
let doc_name = "CommonMark"
let fingerprint = B00_fexts.cmark

(* Outcome builds *)

let uris_rel ~to_dir ~files =
  let add acc f = Fpath.to_uri_path (Fpath.relative ~to_dir f) :: acc in
  List.rev (List.fold_left add [] files)

let md_to_html m ~src_root ~build_dir ~html_dir ~js_files ~css_files md =
  let generator = "brzo %%VERSION%% cmark" in
  let to_dir = Fpath.parent md in
  let scripts = uris_rel ~to_dir ~files:js_files in
  let styles = uris_rel ~to_dir ~files:css_files in
  let o_frag = Fpath.(reroot ~root:src_root ~dst:build_dir md -+ ".htmlf") in
  let o = Fpath.(reroot ~root:src_root ~dst:html_dir md -+ ".html") in
  let opts = Cmd.empty in
  Memo.file_ready m md;
  B00_cmark.to_html m ~opts ~mds:[md] ~generator ~scripts ~styles ~o_frag ~o

let build_web m c _ ~build_dir ~artefact ~srcs =
  let src_root = Brzo.Conf.root c and html_dir = artefact in
  let mds = B00_fexts.(find_files cmark) srcs in
  let files_to_copy = B00_fexts.(find_files www) srcs in
  let js_files = B00_fexts.(find_files js) srcs in
  let css_files = B00_fexts.(find_files css) srcs in
  let md_to_html =
    md_to_html m ~src_root ~build_dir ~html_dir ~js_files ~css_files
  in
  let* () = Memo.mkdir m html_dir in
  List.iter md_to_html mds;
  List.iter (Brzo.Memo.copy_file m ~src_root ~dst_root:html_dir) files_to_copy;
  Fut.return ()

(* Outcomes *)

let exec =
  let name = "exec" and doc = "Build and execute an HTML program (default)." in
  let artefact m c _ ~build_dir =
    Fut.return Fpath.(build_dir / "html")
  in
  let build = build_web in
  let action = Brzo_outcome.Action.show_uri in
  Brzo_outcome.v ~name ~doc ~artefact ~build ~action_has_args:false ~action ()

let doc =
  let name = "doc" and doc = "Synonym for $(b,--exec)." in
  Brzo_outcome.with_outcome ~name ~doc exec

let outcomes = [doc; exec]
let pre_outcomes = List.map Brzo_outcome.pre_outcome outcomes

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
