(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Fut.Syntax

module Conf = struct
  type t = unit
  let tid = Type.Id.make ()
  let conf_get _ = Ok ()
  let keys = String.Set.empty
  let parse = conf_get
  let parse_with_cli = Cmdliner.Term.const conf_get
  let pp = Fmt.nop
end

let name = "cmark"
let doc_name = "CommonMark"
let fingerprint = B0_file_exts.cmark

(* Outcome builds *)

let urls_rel ~to_dir ~files =
  let add acc f = Fpath.to_url_path (Fpath.relative ~to_dir f) :: acc in
  List.rev (List.fold_left add [] files)

let md_to_html m ~src_root ~build_dir ~html_dir ~js_files ~css_files md =
  let generator = "brzo %%VERSION%% cmark" in
  let to_dir = Fpath.parent md in
  let scripts = urls_rel ~to_dir ~files:js_files in
  let styles = urls_rel ~to_dir ~files:css_files in
  let o_frag = Fpath.(reroot ~src_root ~dst_root:build_dir md -+ ".htmlf") in
  let o = Fpath.(reroot ~src_root ~dst_root:html_dir md -+ ".html") in
  let opts = Cmd.empty in
  B0_memo.ready_file m md;
  B0_cmark.to_html m ~opts ~mds:[md] ~generator ~scripts ~styles ~o_frag ~o

let build_web m c _ ~build_dir ~artefact ~srcs =
  let src_root = Brzo.Conf.root c and html_dir = artefact in
  let mds = B0_file_exts.(find_files cmark) srcs in
  let files_to_copy = B0_file_exts.(find_files www) srcs in
  let js_files = B0_file_exts.(find_files js) srcs in
  let css_files = B0_file_exts.(find_files css) srcs in
  let md_to_html =
    md_to_html m ~src_root ~build_dir ~html_dir ~js_files ~css_files
  in
  let* () = B0_memo.mkdir m html_dir in
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
