(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open Fut.Syntax
open Brzo_b0_c

(* Todo best-effort strategy:

   (libs pkg-config-names...)
   (lock-libs)

   Do discovery via includes.

   1. For each include try to guess a package name from the include
   2. Try to lookup package name in pkg-config if that succeeds
      use it for compiling and link step.

   Problem for 1. if the includes are not in the default include
   path doesn't work. Maybe it's better to query all pkg-config packages
   for their includes and try to match them in the sources, problem
   chokes on broken pc files. *)

module Conf = struct
  open B0_sexp
  open Cmdliner

  type t =
    { doxyfile : Fpath.t option;
      use_dot : bool; }

  let doxyfile c = c.doxyfile
  let use_dot c = c.use_dot

  let tid : t Type.Id.t = Type.Id.make ()
  let docs = Brzo.Cli.s_outcome_opts

  let doxyfile_key = "doxyfile"
  let doxyfile_c =
    Brzo.Conf.Bit.with_cli_arg doxyfile_key
      ~doc:"Use $(docv) as the Doxyfile. If unspecified a \
            Doxyfile is generated see $(b,odig doc brzo) for details."
      ~docv:"FILE" ~docs
      ~absent:None
      ~conf:Sexpq.(some (atomic Brzo.Sexp.fpath))
      ~arg:
        Cmdliner.Arg.(opt (some ~none:"generated" (some B0_cli.fpath)) None)

  let use_dot_key = "use-dot"
  let use_dot_c =
    Brzo.Conf.Bit.with_cli_arg use_dot_key
      ~doc:"If the Doxyfile is generated, $(docv) determines if the $(b,dot) \
            tool is used to generate dependency and call graphs."
      ~docv:"BOOL" ~docs
      ~absent:true
      ~conf:Sexpq.(atomic bool)
      ~arg:(Arg.(opt ~vopt:(Some true) (some ~none:"true" bool) None))

  let get doxyfile use_dot sexp =
    Result.bind (Brzo.Conf.Bit.get doxyfile_c doxyfile sexp) @@ fun doxyfile ->
    Result.bind (Brzo.Conf.Bit.get use_dot_c use_dot sexp) @@ fun use_dot ->
    Ok { doxyfile; use_dot }

  let keys = String.Set.of_list [doxyfile_key; use_dot_key]
  let parse = get None None
  let parse_with_cli =
    Term.(const get $ Brzo.Conf.Bit.cli doxyfile_c $
          Brzo.Conf.Bit.cli use_dot_c)

  let pp =
    Fmt.record
      [ Fmt.field "doxyfile" doxyfile (Brzo.Conf.pp_auto Fpath.pp_unquoted);
        Fmt.field "use-dot" use_dot Fmt.bool ]
end

let name = "c"
let doc_name = "C"
let fingerprint = B0_file_exts.c_lang

(* Exec outcome *)

type builder =
  { m : B0_memo.t;
    c : Brzo.Conf.t;
    dc : Conf.t;
    src_root : Fpath.t;
    srcs : B0_file_exts.map;
    build_dir : Fpath.t; }

let builder m c dc ~build_dir ~srcs =
  let src_root = Brzo.Conf.root c in
  Fut.return { m; c; dc; src_root; srcs; build_dir }

let default_flags = Cmd.(arg "-g" % "-Wall")

let compile_src b ~in_dir ~obj_ext ~deps cname c =
  let d = Fpath.(in_dir / Fmt.str "%s%s" cname ".d") in
  let o = Fpath.(in_dir / Fmt.str "%s%s" cname obj_ext) in
  begin
    B0_memo.file_ready b.m c;
    Inc_deps.write b.m ~src:c ~o:d;
    ignore @@
    let* deps = Inc_deps.read b.m ~src:c d in
    List.iter (B0_memo.file_ready b.m) deps;
    Compile.c_to_o ~args:default_flags b.m ~deps ~c ~o;
    Fut.return ()
  end;
  o

let compile_srcs b ~in_dir =
  let* obj_ext = Brzo_b0_c.Conf.obj_ext b.m in
  let rec loop os cunits hs = function
  | [] -> Fut.return os
  | c :: cs ->
      let cname = Fpath.basename ~no_ext:true c in
      match String.Map.find cname cunits with
      | exception Not_found ->
          let o = compile_src b ~in_dir ~obj_ext ~deps:hs cname c in
          loop (o :: os) (String.Map.add cname c cunits) hs cs
      | f ->
          (* TODO error message *)
          B0_memo.notify b.m `Warn
            "@[<v>%a:@,File ignored. %a's compilation unit already defined \
             by file:@,%a:@]"
            Fpath.pp_unquoted c Fmt.(code string) cname Fpath.pp_unquoted f;
          loop os cunits hs cs
  in
  let hs = B0_file_exts.(find_files (ext ".h") b.srcs) in
  let cs = B0_file_exts.(find_files (ext ".c") b.srcs) in
  List.iter (B0_memo.file_ready b.m) hs;
  loop [] String.Map.empty hs cs

let build_exe b ~exe =
  let* objs = compile_srcs b ~in_dir:b.build_dir in
  Link.exe b.m ~args:default_flags ~objs ~o:exe;
  Fut.return ()

let exec_build m c dc ~build_dir ~artefact ~srcs =
  let* () = Brzo.Memo.ensure_exec_build m ~srcs ~need_ext:".c" in
  let* b = builder m c dc ~build_dir ~srcs in
  build_exe b ~exe:artefact

let exec =
  let name = "exec" and doc = "Build and execute a program (default)." in
  let artefact m _ _ ~build_dir =
    Fut.return Fpath.(build_dir / "a.out")
  in
  let build = exec_build and action = Brzo_outcome.Action.exec in
  Brzo_outcome.v ~name ~doc ~artefact ~build ~action_has_args:true ~action ()


(* Doc outcome (doxygen) *)

let doxy_qpath f = Fmt.str "\"%s\"" (Fpath.to_string f)
let doxy_srcs srcs =
  let srcs = List.map doxy_qpath srcs in
  (String.concat " \\\n" srcs) ^ "\n"

let gen_doxyfile ~project_name ~version ~root ~out_dir ~use_dot ~srcs =
  Fmt.str
{|
PROJECT_NAME           = "%s"
PROJECT_NUMBER         = "%s"
OUTPUT_DIRECTORY       = %s
STRIP_FROM_PATH        = %s
OPTIMIZE_OUTPUT_FOR_C  = YES
EXTRACT_ALL            = YES
QUIET                  = YES
INPUT                  = %s

GENERATE_HTML          = YES
HTML_OUTPUT            = html
HTML_FILE_EXTENSION    = .html
HTML_EXTRA_STYLESHEET  =
HTML_EXTRA_FILES       =
HTML_DYNAMIC_SECTIONS  = NO
DISABLE_INDEX          = YES
GENERATE_TREEVIEW      = YES
SEARCHENGINE           = YES
SOURCE_BROWSER         = YES
INLINE_SOURCES         = YES

HAVE_DOT               = %s
INCLUDED_BY_GRAPH      = YES
CALL_GRAPH             = YES
CALLER_GRAPH           = YES
DIRECTORY_GRAPH        = YES
DOT_IMAGE_FORMAT       = svg
INTERACTIVE_SVG        = YES

GENERATE_LATEX         = NO
|}
project_name version (doxy_qpath out_dir) (doxy_qpath root) (doxy_srcs srcs)
(if use_dot then "YES" else "NO")

let override_doxyfile ~out_dir ~srcs doxy =
  Fmt.str
{|
%s
OUTPUT_DIRECTORY = %s
INPUT = %s
|}
doxy (doxy_qpath out_dir) (doxy_srcs srcs)

let find_doxyfile m c dc = match Conf.doxyfile dc with
| None ->
    let doxyfile = Fpath.(Brzo.Conf.root c / "Doxyfile") in
    if Os.File.exists doxyfile |> B0_memo.fail_if_error m
    then Some doxyfile else None
| Some doxyfile ->
    match Os.File.exists doxyfile |> B0_memo.fail_if_error m with
    | true -> Some doxyfile
    | false ->
        B0_memo.fail m "Doxyfile %a not found."
          Fmt.(code Fpath.pp_unquoted) doxyfile

let vcs_version ~root =
  Log.if_error ~use:"" @@
  Result.bind (B0_vcs_repo.find ~dir:root ()) @@ function
  | None -> Ok ""
  | Some vcs -> B0_vcs_repo.describe vcs ~dirty_mark:true "HEAD"

let write_doxyfile m c dc ~out_dir ~srcs ~o =
  ignore @@ match find_doxyfile m c dc with
  | Some doxyfile ->
      B0_memo.file_ready m doxyfile;
      let* doxy =  B0_memo.read m doxyfile in
      let doxy = override_doxyfile ~out_dir ~srcs doxy in
      (B0_memo.write m ~reads:[doxyfile] ~stamp:doxy o @@ fun () -> Ok doxy);
      Fut.return ()
  | None ->
      let root = Brzo.Conf.root c in
      let project_name = Fpath.basename root in
      let version = vcs_version ~root in
      let* use_dot =
        if not (Conf.use_dot dc) then Fut.return false else
        let* dot = B0_memo.tool_opt m Brzo_b0_doxygen.Tool.dot in
        match dot with
        | Some _ -> Fut.return true
        | None ->
            B0_memo.notify
              m `Warn "@[<v>Tool %a not found.@,Use option %a or invoke %a@,to \
                       disable this warning.@]"
              Fmt.(code string) "dot" Fmt.(code string) "--use-dot=false"
              Fmt.(code string) "brzo file set c.use-dot false";
            Fut.return false
      in
      let doxy =
        gen_doxyfile ~project_name ~version ~root ~out_dir ~use_dot ~srcs
      in
      B0_memo.write m ~reads:srcs ~stamp:doxy o (fun () -> Ok doxy);
      Fut.return ()

let doc_build m c dc ~build_dir ~artefact ~srcs =
  let out_dir = Fpath.(build_dir / "o") in
  let srcs_exts = B0_file_exts.(ext ".md" + ext ".h" + ext ".c") in
  let srcs = B0_file_exts.find_files srcs_exts srcs in
  List.iter (B0_memo.file_ready m) srcs;
  let doxyfile = Fpath.(build_dir / "Doxyfile") in
  write_doxyfile m c dc ~out_dir ~srcs ~o:doxyfile;
  let doxygen = B0_memo.tool m Brzo_b0_doxygen.Tool.doxygen in
  B0_memo.spawn' m ~reads:(doxyfile :: srcs) ~writes_root:out_dir @@
  doxygen Cmd.(path doxyfile);
  Fut.return ()

let doc =
  let name = "doc" and doc = "Build and show source documentation (doxygen)." in
  let artefact m _ _ ~build_dir =
    Fut.return Fpath.(build_dir / "o" / "html")
  in
  let build = doc_build and action = Brzo_outcome.Action.show_uri in
  Brzo_outcome.v ~name ~doc ~artefact ~build ~action_has_args:false ~action ()

let html =
  let name = "html" in
  let doc = "Build and execute a HTML program (emscripten)." in
  let artefact m _ _ ~build_dir =
    Fut.return Fpath.(build_dir / "index.html")
  in
  let build m c _ ~build_dir ~artefact ~srcs = failwith "TODO" in
  let action _ c _ ~build_dir ~artefact = failwith "TODO" in
  Brzo_outcome.v ~name ~doc ~artefact ~build ~action_has_args:true ~action ()

(* Outcomes *)

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
