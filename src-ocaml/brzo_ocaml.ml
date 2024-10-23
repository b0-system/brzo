(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open Fut.Syntax
open Brzo_b0_ocaml
open Brzo_b0_js_of_ocaml

(* A few things that could be tested for improvement.

   * Currently `ocamldep` is done once on all of the sources. For incremental
     rebuilds this may add a large constant factor.  *)

(* Domain configuration *)

module Conf = Brzo_ocaml_conf

(* Domain definition *)

let name = "ocaml"
let doc_name = "OCaml"
let fingerprint = B0_file_exts.ocaml_lang

(* Module resolver *)

let default_dep_dirs m ocaml_conf =
  let* lib_dir = Brzo_b0_opam.(if_exists m (lib_dir m)) in
  let stdlib_dir = B0_ocaml.Conf.where ocaml_conf in
  let dep_dirs = stdlib_dir :: Option.to_list lib_dir in
  Fut.return dep_dirs

let resolver m c dc ~memo_dir ocaml_conf =
  let dep_dirs m dc = match Brzo_ocaml_conf.ocamlpath dc with
  | [] -> default_dep_dirs m ocaml_conf
  | dep_dirs -> Fut.return dep_dirs
  in
  let* dep_dirs = dep_dirs m dc in
  let lock_deps = Brzo_ocaml_conf.lock_libs dc in
  let deps = Brzo_ocaml_conf.libs dc in
  let deps = if lock_deps then `Locked deps else `Open deps in
  let r, miss_deps = Mod_resolver.create m ~memo_dir ~dep_dirs deps in
  let* () = Brzo_ocaml_be.handle_miss_user_deps r miss_deps in
  Fut.return r

let ocaml_conf m dc ~build_dir =
  let default_native_if = B0_ocaml.Tool.ocamlopt in
  let t = Brzo_ocaml_conf.target dc in
  let* target = Brzo_ocaml_conf.default_target ~default_native_if m t in
  let comp = match target with
  | `Native -> B0_ocaml.Tool.ocamlopt
  | _ -> B0_ocaml.Tool.ocamlc
  in
  let o = Fpath.(build_dir / "ocaml.conf") in
  B0_ocaml.Conf.write m ~comp ~o;
  B0_ocaml.Conf.read m o

let comp_of_code = function
| `Native -> B0_ocaml.Tool.ocamlopt
| `Byte -> B0_ocaml.Tool.ocamlc

(* Builder *)

type builder =
  { m : B0_memo.t;
    c : Brzo.Conf.t;
    dc : Brzo_ocaml_conf.t;
    ocaml_conf : B0_ocaml.Conf.t;
    src_root : Fpath.t;
    srcs : B0_file_exts.map;
    build_dir : Fpath.t;
    mutable ext_incs : Fpath.Set.t; (* For merlin. Try to streamline that *)
    r : Mod_resolver.t; }

let builder m c dc ~build_dir ~srcs =
  let src_root = Brzo.Conf.root c in
  let* ocaml_conf = ocaml_conf m dc ~build_dir in
  let* r = resolver m c dc ~memo_dir:build_dir ocaml_conf in
  let ext_incs = Fpath.Set.empty in
  Fut.return @@
  { m; c; dc; ocaml_conf; build_dir; src_root; srcs; ext_incs; r }

let record_ext_incs b ext_objs = (* XXX ugly *)
  let add_inc acc obj = Fpath.Set.add (Fpath.parent obj) acc in
  b.ext_incs <- List.fold_left add_inc b.ext_incs ext_objs

let pp_try_install pkgs ppf n =
  (* FIXME spell check against existing compilation units. *)
  let pkgs = match Brzo_ocaml_be.suggest_pkgs_for_modname n ~pkgs with
  | `None -> [] | `Prefix_match pkg -> [pkg] | `Fuzzy_prefix_match pkgs -> pkgs
  in
  match pkgs with
  | [] ->
      Fmt.pf ppf "@[Module %a could not be resolved.@]" B0_ocaml.Modname.pp n
  | pkgs ->
      let oneof = match pkgs with [_] -> "the" | _ -> "one of the" in
      let pp_install ppf pkg =
        Fmt.code ppf (String.concat " " ["opam"; "install"; pkg])
      in
      Fmt.pf ppf
        "@[<v>Module %a could not be resolved.@,\
         Maybe try %s following package install:@,@,@[<v>%a@]@,@]"
        B0_ocaml.Modname.pp n oneof Fmt.(list pp_install) pkgs

let mention_unresolved_on_fail deps o =
  if B0_ocaml.Modname.Set.is_empty deps then () else
  match B0_zero.Op.Spawn.exit (B0_zero.Op.Spawn.get o) with
  | None | Some (`Signaled _) | Some (`Exited 0)-> ()
  | Some (`Exited _) ->
      let deps = B0_ocaml.Modname.Set.elements deps in
      (* post_exec fiddling with memo to invoke opam is not a good idea. Maybe
         gather errors in the builder and say something at the end ?
         For now we simply invoke opam in place. *)
      let pkgs = Brzo_b0_opam.pkg_list () |> Log.if_error ~use:[] in
      let err = Fmt.str "@[<v>@,%a@]@." (Fmt.list (pp_try_install pkgs)) deps in
      B0_zero.Op.set_status o (B0_zero.Op.Failed (B0_zero.Op.Exec (Some err)))

let compile_c_srcs b ~comp ~opts ~in_dir =
  (* XXX Maybe better things could be done here once we have a good C
     domain. *)
  let obj_ext = B0_ocaml.Conf.obj_ext b.ocaml_conf in
  let rec loop os cunits hs = function
  | [] -> List.rev os
  | c :: cs ->
      let cname = Fpath.basename ~strip_ext:true c in
      match String.Map.find cname cunits with
      | exception Not_found ->
          let o = Fpath.(in_dir / Fmt.str "%s%s" cname obj_ext) in
          B0_memo.ready_file b.m c;
          ignore @@ B0_ocaml.Compile.c_to_o b.m ~comp ~opts ~reads:hs ~c ~o;
          loop (o :: os) (String.Map.add cname c cunits) hs cs
      | f ->
          B0_memo.notify b.m `Warn
            "@[<v>%a:@,File ignored. %s's compilation unit already defined \
             by file:@,%a:@]"
            Fpath.pp_unquoted c cname Fpath.pp_unquoted f;
          loop os cunits hs cs
  in
  let hs = B0_file_exts.(find_files (ext ".h") b.srcs) in
  let cs = B0_file_exts.(find_files (ext ".c") b.srcs) in
  B0_memo.ready_files b.m hs;
  Fut.return (loop [] String.Map.empty hs cs)

let compile_intf b ~comp ~opts ~build_dir ~local_mods msrc =
  match B0_ocaml.Modsrc.mli msrc with
  | None -> None
  | Some mli ->
      let o = B0_ocaml.Modsrc.cmi_file msrc in
      begin
        ignore @@
        let deps = B0_ocaml.Modsrc.mli_deps msrc in
        let* local_objs, ext_objs, unresolved, ambs =
          Brzo_ocaml_be.resolve_intf_deps b.r ~in_dir:build_dir ~local_mods deps
        in
        record_ext_incs b ext_objs;
        let* () = Brzo_ocaml_be.handle_amb_deps b.r mli ~unresolved ambs in
        let reads = List.rev_append local_objs ext_objs in
        let post_exec = mention_unresolved_on_fail unresolved in
        ignore @@ B0_ocaml.Compile.mli_to_cmi
          b.m ~post_exec ~comp ~opts ~reads ~mli ~o ~and_cmti:true;
        Fut.return ()
      end;
      Some o

let compile_impl b ~code ~opts ~build_dir ~local_mods msrc =
  match B0_ocaml.Modsrc.ml msrc with
  | None -> None
  | Some ml ->
      let o = Option.get (B0_ocaml.Modsrc.impl_file ~code msrc) in
      begin
        ignore @@
        let deps = B0_ocaml.Modsrc.ml_deps msrc in
        let* local_objs, ext_objs, unresolved, ambs =
          Brzo_ocaml_be.resolve_impl_deps b.r ~code ~in_dir:build_dir
            ~local_mods deps
        in
        record_ext_incs b ext_objs;
        let* () = Brzo_ocaml_be.handle_amb_deps b.r ml ~unresolved ambs in
        let has_cmi, local_objs = match B0_ocaml.Modsrc.mli msrc with
        | None -> false, local_objs
        | Some _ -> true, B0_ocaml.Modsrc.cmi_file msrc :: local_objs
        in
        let reads = List.rev_append ext_objs local_objs in
        let post_exec = mention_unresolved_on_fail unresolved in
        ignore @@ B0_ocaml.Compile.ml_to_impl b.m
          ~post_exec ~code ~opts ~has_cmi ~reads ~ml ~o ~and_cmt:true;
        Fut.return ()
      end;
      Some o

let compile_intfs b ~comp ~opts ~build_dir ~local_mods =
let compile _ msrc acc =
    match compile_intf b ~comp ~opts ~local_mods ~build_dir msrc with
    | None -> acc | Some cmi -> cmi :: acc
  in
  String.Map.fold compile local_mods []

let compile_impls b ~code ~opts ~build_dir ~local_mods =
  let compile _ m acc =
    match compile_impl b ~code ~opts ~local_mods ~build_dir m with
    | None -> acc | Some o -> o :: acc
  in
  String.Map.fold compile local_mods []

let local_mods ?(more_srcs = []) ~opts ~build_dir ~mli_only b =
  let maybe_ml = if mli_only then String.Set.empty else B0_file_exts.(ext ".ml") in
  let src_exts = B0_file_exts.(ext ".mli" + maybe_ml) in
  let srcs = B0_file_exts.find_files src_exts b.srcs in
  let o = Fpath.(b.build_dir / "brzo.ocaml.compdep") in
  B0_memo.ready_files b.m srcs;
  let srcs = List.rev_append more_srcs srcs in
  B0_ocaml.Modsrc.Deps.write b.m ~src_root:b.src_root ~srcs ~o;
  let* src_deps = B0_ocaml.Modsrc.Deps.read b.m ~src_root:b.src_root o in
  Fut.return (B0_ocaml.Modsrc.map_of_srcs b.m ~build_dir ~src_deps ~srcs)

let compile_srcs ?more_srcs b ~code ~opts ~build_dir =
  let comp = comp_of_code code in
  let* c_objs = compile_c_srcs b ~comp ~opts ~in_dir:build_dir in
  let* local_mods = local_mods ?more_srcs ~opts  ~build_dir ~mli_only:false b in
  let _cmis = compile_intfs b ~comp ~build_dir ~opts ~local_mods in
  let cobjs = compile_impls b ~code ~build_dir ~opts ~local_mods in
  Fut.return (c_objs, cobjs)

let find_link_deps b ~code ~in_dir cobjs =
  let o = Fpath.(in_dir / "brzo.ocaml.linkdep") in
  B0_ocaml.Cobj.write b.m ~cobjs ~o;
  let ext = match code with `Byte -> ".cma" | `Native -> ".cmxa" in
  let* cobjs, ext_deps =
    Fut.map B0_ocaml.Cobj.sort (B0_ocaml.Cobj.read b.m o)
  in
  let* ext_objs = Mod_resolver.find_rec_impls_for_modrefs b.r ~ext ext_deps in
  Fut.return (cobjs, ext_objs)

let write_merlin_file b =
  (* XXX Not very fond of pooping merlin files outside the _b0 directory.
     XXX Add support for out of _b0 cleaning outcome, see also latex's
     domain  generated .bib *)
  let merlin_file = Fpath.(Brzo.Conf.root b.c / ".merlin") in
  let write b =
    let incs = Fpath.Set.add (Fpath.add_dir_sep b.build_dir) b.ext_incs in
    let add_inc p acc = Fmt.str "B %a**" Fpath.pp_unquoted p :: acc in
    let incs = Fpath.Set.fold add_inc incs [] in
    let merlin = "# Generated by brzo" :: "S ./**" :: incs in
    let merlin = String.concat "\n" merlin in
    B0_memo.write b.m ~stamp:merlin merlin_file @@ fun () -> Ok merlin
  in
  match B0_memo.fail_if_error b.m (Os.File.exists merlin_file) with
  | false -> write b
  | true ->
      (* Memo weakness: updates. This declares .merlin as being ready but
         then writes it. If an op had to rely on .merlin that would be bad. *)
      B0_memo.ready_file b.m merlin_file;
      ignore @@
      let* s = B0_memo.read b.m merlin_file in
      (if String.starts_with ~prefix:"# Generated by brzo" s
       then write b else ());
      Fut.return ()

(* Exec outcome *)

let drop_stdlib_and_adjust_ext_cobjs b cobjs =
  (* FIXME hack, needs a rework since 4.12 C archive
     of cmxas may not exist so we need to pass them in B00_ocaml's `Link.code`
     cobjs if they exist, we need to redo find_link_deps to include them
     or maybe rework around Lib. Also why do we drop the stdlib ? *)
  let is_stdlib n = String.equal "stdlib" n in
  let lib_ext = B0_ocaml.Conf.lib_ext b.ocaml_conf in
  let rec loop b rfiles = function
  | [] -> rfiles
  | cobj :: cobjs ->
      match is_stdlib
              (Fpath.basename ~strip_ext:true (B0_ocaml.Cobj.file cobj))
      with
      | true -> loop b rfiles cobjs
      | false ->
          match Fpath.has_ext ".cmxa" (B0_ocaml.Cobj.file cobj) with
          | false -> loop b (B0_ocaml.Cobj.file cobj :: rfiles) cobjs
          | true ->
              let clib = Fpath.set_ext lib_ext (B0_ocaml.Cobj.file cobj) in
              match Os.File.exists clib |> B0_memo.fail_if_error b.m with
              | false -> loop b (B0_ocaml.Cobj.file cobj :: rfiles) cobjs
              | true -> loop b (B0_ocaml.Cobj.file cobj :: clib :: rfiles) cobjs
  in
  loop b [] cobjs

let build_exe ?(opts = Cmd.empty) b ~code ~exe =
  let in_dir = b.build_dir in
  let opts = Cmd.(arg "-g" %% opts) in
  let* c_objs, cobjs = compile_srcs b ~code ~opts ~build_dir:in_dir in
  let* cobjs, ext_cobjs = find_link_deps b ~code ~in_dir cobjs in
  let rev_ext_cobjs = drop_stdlib_and_adjust_ext_cobjs b ext_cobjs in
  let cobjs =
    List.rev_append rev_ext_cobjs (List.map B0_ocaml.Cobj.file cobjs)
  in
  write_merlin_file b;
  B0_ocaml.Link.code b.m ~conf:b.ocaml_conf ~code ~opts ~c_objs ~cobjs ~o:exe;
  Fut.return ()

let js_exe = "a.js"
let js_tty_glue = "brzo_tty_glue.js"

let no_check_prims = Cmd.(arg "-no-check-prims")

let build_node_exe b ~artefact =
  let src_root = b.src_root in
  let byte_exe = Fpath.(b.build_dir / "a.out") in
  let ocaml_js = Fpath.(b.build_dir / "a.out.js") in
  let js_files = B0_file_exts.(find_files js) b.srcs in
  let jss =
    let copy acc src =
      let dst = Fpath.reroot ~src_root ~dst_root:b.build_dir src in
      B0_memo.ready_file b.m src;
      B0_memo.copy b.m src ~dst;
      dst :: acc
    in
    List.rev (ocaml_js :: (List.fold_left copy [] js_files))
  in
  let args = Cmd.(arg "--source-map-inline") in
  ignore (build_exe b ~opts:no_check_prims ~code:`Byte ~exe:byte_exe);
  Js_of_ocaml.compile b.m ~byte_exe ~args ~o:ocaml_js;
  Js_of_ocaml.link b.m ~jss ~args ~o:artefact;
  Fut.return ()

let build_html_web b ~toplevel_css ~js_exe =
  let src_root = b.src_root and dst_root = b.build_dir in
  let index = Fpath.(src_root / "index.html") in
  let files_to_copy = B0_file_exts.(find_files www) b.srcs in
  List.iter (Brzo.Memo.copy_file b.m ~src_root ~dst_root) files_to_copy;
  match List.mem index files_to_copy with
  | true -> ()
  | false ->
      let rel_uris srcs =
        let add_rel acc f = Option.get (Fpath.strip_prefix src_root f) :: acc in
        let rels = List.fold_left add_rel [] srcs in
        List.rev_map Fpath.to_url_path rels
      in
      let generator = "brzo %%VERSION%% ocaml" in
      let styles = rel_uris B0_file_exts.(find_files css b.srcs) in
      let scripts = rel_uris B0_file_exts.(find_files js b.srcs) in
      let scripts = scripts @ [js_tty_glue; js_exe] in
      let index = Fpath.(dst_root / "index.html") in
      let title = Fpath.basename src_root in
      Js_of_ocaml.write_page
        b.m ~toplevel_css ~generator ~scripts ~styles ~title ~o:index

let build_html_exe b ~artefact =
  let byte_exe = Fpath.(b.build_dir / "a.out") in
  let args = Cmd.(arg "--source-map-inline" % "--extern-fs") in
  ignore (build_exe b ~opts:no_check_prims ~code:`Byte ~exe:byte_exe);
  Js_of_ocaml.compile b.m ~args ~byte_exe ~o:Fpath.(b.build_dir / js_exe);
  build_html_web b ~toplevel_css:false ~js_exe;
  Fut.return ()

let exec_html_action m c dc ~build_dir ~artefact =
  let tty_glue () =
    (* FIXME use memo *)
    Result.bind (Os.Env.current ()) @@ fun env ->
    let tty_glue =
      let cwd = Brzo.Conf.cwd c and args = Brzo.Conf.action_args c in
      let exe = Fpath.(build_dir / js_exe) in
      Log.time (fun _ m -> m "Generating tty glue") @@ fun () ->
      Js_of_ocaml.tty_glue ~exe ~cwd ~env ~args
    in
    let js = Fpath.(build_dir / js_tty_glue) in
    Os.File.write ~make_path:true ~force:true js tty_glue
  in
  match tty_glue () with
  | Error e -> Fut.return (fun () -> Error e)
  | Ok () -> Brzo_outcome.Action.show_uri m c dc ~build_dir ~artefact

let exec_node_action m c dc ~build_dir ~artefact =
  Fut.return @@ fun () ->
  let action_args = Brzo.Conf.action_args c in
  Result.bind (Os.Cmd.get (Cmd.tool "node")) @@ fun node ->
  let cmd = Cmd.(node %% path artefact %% list (action_args)) in
  Ok (Os.Exit.execv cmd)

let exec_target m dc =
  let t = Brzo_ocaml_conf.target dc in
  Brzo_ocaml_conf.default_target ~default_native_if:B0_ocaml.Tool.ocamlopt m t

let exec_build_dir_suff m _ dc  =
  let* target = exec_target m dc in
  Fut.return ("-" ^ Brzo_ocaml_conf.target_to_string target)

let exec_build m c dc ~build_dir ~artefact ~srcs =
  let* () = Brzo.Memo.ensure_exec_build m ~srcs ~need_ext:".ml" in
  let* b = builder m c dc ~build_dir ~srcs in
  let* target = exec_target m dc in
  match target with
  | `Byte -> build_exe b ~code:`Byte ~exe:artefact
  | `Native -> build_exe b ~code:`Native ~exe:artefact
  | `Html -> build_html_exe b ~artefact
  | `Node -> build_node_exe b ~artefact

let exec_artefact m c dc ~build_dir = match Brzo_ocaml_conf.target dc with
| None | Some (`Byte | `Native) ->
    (* FIXME this will write in build_dir I'm not sure the protocol
       allows that. *)
    let* ocaml_conf = ocaml_conf m dc ~build_dir in
    let exe_ext = B0_ocaml.Conf.exe_ext ocaml_conf in
    Fut.return Fpath.(build_dir / Fmt.str "a.out%s" exe_ext)
| Some `Html -> Fut.return Fpath.(build_dir / "index.html")
| Some `Node -> Fut.return Fpath.(build_dir / js_exe)

let exec_action m c dc ~build_dir ~artefact =
  match Brzo_ocaml_conf.target dc with
  | None | Some (`Byte | `Native) ->
      Brzo_outcome.Action.exec m c dc ~build_dir ~artefact
  | Some `Html -> exec_html_action m c dc ~build_dir ~artefact
  | Some `Node -> exec_node_action m c dc ~build_dir ~artefact

let exec =
  let name = "exec" in
  let doc = "Build and execute a program (default)." in
  let build_dir_suff = exec_build_dir_suff in
  let artefact = exec_artefact in
  let build = exec_build in
  let action = exec_action in
  Brzo_outcome.v
    ~name ~doc ~build_dir_suff ~artefact ~build ~action_has_args:true ~action ()

(* Top and utop outcome *)

let find_rec_impls_for_modname r ~ext modname =
  Fut.bind (Mod_resolver.find_cmis_for_modname r modname) @@ function
  | [] -> Fut.return []
  | cmi :: cmis ->
      if cmis <> [] then
        (* TODO *)
        Log.warn begin fun m ->
          m "@[<v>%a: ignored candidates:@,%a@]"
            B0_ocaml.Modname.pp modname Fmt.(list Brzo_ocaml_cmi.pp) cmis
        end;
      let modrefs =
        B0_ocaml.Modref.Set.singleton (Brzo_ocaml_cmi.modref cmi)
      in
      Mod_resolver.find_rec_impls_for_modrefs r ~ext modrefs

let drop_top_libs r ~code ~top cobjs =
  (* Filter out libs that are already linked in toplevels. *)
  let drop_libs_for_modname r ~ext modname objs =
    let* libs = find_rec_impls_for_modname r ~ext modname in
    let add_lib acc o = Fpath.Set.add (B0_ocaml.Cobj.file o) acc in
    let libs = List.fold_left add_lib Fpath.Set.empty libs in
    let not_in_libs libs o = not (Fpath.Set.mem (B0_ocaml.Cobj.file o) libs) in
    Fut.return (List.filter (not_in_libs libs) objs)
  in
  match code with
  | `Native ->
      drop_libs_for_modname r ~ext:".cmxa"
        (B0_ocaml.Modname.v "Opttoploop") cobjs
  | `Byte when top = "utop" ->
      drop_libs_for_modname r ~ext:".cma" (B0_ocaml.Modname.v "UTop") cobjs
  | `Byte ->
      drop_libs_for_modname r ~ext:".cma" (B0_ocaml.Modname.v "Toploop") cobjs

let write_top_cmd
    ?args:(al = Cmd.empty) m ~top ~build_dir ~incs ~libs ~archive ~artefact
  =
  let incs = Cmd.paths ~slip:"-I" (build_dir :: incs) in
  let libs = Cmd.paths libs in
  let cmd = Cmd.(arg top %% al %% incs %% libs %% path archive) in
  let cmd = String.concat "\n" (Cmd.to_list cmd) in
  B0_memo.write m ~stamp:cmd artefact @@ fun () -> Ok cmd

let run_top_cmd m c top_cmd =
  Fut.return @@ fun () ->
  Result.bind (Os.File.read top_cmd) @@ fun contents ->
  match String.cuts_left ~sep:"\n" contents with
  | [] -> Fmt.error "%a: no command could be parsed" Fpath.pp_quoted top_cmd
  | cmd :: args ->
      let aargs = Brzo.Conf.action_args c in
      match Os.Cmd.get (Cmd.tool cmd) with
      | Error _ as e -> e
      | Ok cmd -> Ok (Os.Exit.execv Cmd.(cmd %% list args %% list aargs))

let cobjs_incs objs =
  (* This may not work with library variant depending on how they
     are installed. FIXME test on a dune example. *)
  let add_inc acc o = Fpath.parent (B0_ocaml.Cobj.file o) :: acc in
  Fpath.distinct @@ List.rev (List.fold_left add_inc [] objs)

let build_top b ~code ~top ~artefact =
  let in_dir = b.build_dir and oname = "brzo_top" in
  let archive = match code with
  | `Byte -> Fpath.(in_dir / Fmt.str "%s.cma" oname)
  | `Native -> Fpath.(in_dir / Fmt.str "%s.cmxs" oname)
  in
  let opts = Cmd.arg "-g" in
  let* c_objs, cobjs = compile_srcs b ~code ~opts ~build_dir:in_dir in
  let has_cstubs = c_objs <> [] in
  if has_cstubs
  then (ignore @@ B0_ocaml.Archive.cstubs b.m
      ~conf:b.ocaml_conf ~opts ~c_objs ~odir:in_dir ~oname);
  let* cobjs, ext_cobjs = find_link_deps b ~in_dir ~code cobjs in
  let cobjs =
    List.map B0_ocaml.Cobj.file cobjs and incs = cobjs_incs ext_cobjs
  in
  write_merlin_file b;
  ignore @@
  B0_ocaml.Archive.code
    b.m ~conf:b.ocaml_conf ~code ~opts ~has_cstubs ~cobjs ~odir:in_dir ~oname;
  begin match code with
  | `Byte -> ()
  | `Native ->
      let cmxa = Fpath.(in_dir / Fmt.str "%s.cmxa" oname) in
      ignore @@ B0_ocaml.Archive.native_dynlink
        b.m ~conf:b.ocaml_conf ~opts ~has_cstubs ~cmxa ~o:archive;
  end;
  let* ext_cobjs = drop_top_libs b.r ~code ~top ext_cobjs in
  let args, libs = match code with
  | `Byte -> Cmd.empty, List.map B0_ocaml.Cobj.file ext_cobjs
  | `Native ->
      let cmxa_to_cmxs_file o = Fpath.set_ext ".cmxs" (B0_ocaml.Cobj.file o) in
      Cmd.(arg "-noinit"), (* most .ocamlinit will fail *)
      List.map cmxa_to_cmxs_file ext_cobjs
  in
  write_top_cmd
    b.m ~args ~top ~build_dir:b.build_dir ~incs ~libs ~archive ~artefact;
  Fut.return ()

let build_html_top b ~top ~artefact =
  let write_mod_names m objs ~o =
    (* FIXME this is slightly wrong and leads to a jsoo warning
       we need to check if corresponding cmis exist, they may be hidden
       on purpose (happens often on erratique packages) *)
    let reads = List.rev_map B0_ocaml.Cobj.file objs in
    B0_memo.write m ~reads o @@ fun () ->
    let add_defs acc o =
      B0_ocaml.Modref.Set.union (B0_ocaml.Cobj.defs o) acc
    in
    let defs = List.fold_left add_defs B0_ocaml.Modref.Set.empty objs in
    let mods =
      B0_ocaml.Modref.Set.fold (fun r acc -> B0_ocaml.Modref.name r :: acc)
        defs []
    in
    Ok (String.concat "\n" mods)
  in
  let write_toplevel_ui_src m ~o =
    B0_memo.write m ~stamp:Js_of_ocaml.toplevel_ui_src o @@
    fun () -> Ok (Js_of_ocaml.toplevel_ui_src)
  in
  let in_dir = b.build_dir and code = `Byte in
  let byte_exe = Fpath.(in_dir / "a.out") in
  let mod_names = Fpath.(in_dir / "top_mod_names") in
  let toplevel_ui_ml = Fpath.(b.build_dir / "brzo_jsoo_toplevel_ui.ml") in
  let args = Cmd.(arg "--extern-fs") in
  let more_srcs = [toplevel_ui_ml] in
  write_toplevel_ui_src b.m ~o:toplevel_ui_ml;
  let opts = Cmd.(arg "-g" %% no_check_prims) in
  let* c_objs, cmos = compile_srcs ~more_srcs b ~code ~opts ~build_dir:in_dir in
  let* cmos, cmas = find_link_deps b ~code ~in_dir cmos in
  write_mod_names b.m (List.rev_append cmos cmas) ~o:mod_names;
  let rev_cmas = drop_stdlib_and_adjust_ext_cobjs b cmas in
  let cmos = List.rev_append rev_cmas (List.map B0_ocaml.Cobj.file cmos) in
  write_merlin_file b;
  B0_ocaml.Link.byte b.m
    ~conf:b.ocaml_conf ~opts ~c_objs ~cobjs:cmos ~o:byte_exe;
  Js_of_ocaml.compile_toplevel b.m ~args ~byte_exe ~mod_names
    ~o:Fpath.(b.build_dir / js_exe);
  build_html_web b ~toplevel_css:true ~js_exe;
  Fut.return ()

let fail_unsupported_target m ~outcome:o ~target:t =
  B0_memo.fail m "%s outcome does not support the --%s target" o t

let top_artefact m c dc ~build_dir =
  Fut.return @@
  match Brzo_ocaml_conf.target dc with
  | None | Some (`Byte | `Native) -> Fpath.(build_dir / "top.cmd")
  | Some `Html -> Fpath.(build_dir / "index.html")
  | Some `Node -> Fpath.(build_dir / js_exe)

let top_target m dc =
  let t = Brzo_ocaml_conf.target dc in
  Brzo_ocaml_conf.default_target ~default_native_if:B0_ocaml.Tool.ocamlnat m t

let top_build_dir_suff m _ dc =
  let* target = top_target m dc in
  Fut.return ("-" ^ Brzo_ocaml_conf.target_to_string target)

let top_build m c dc ~build_dir ~artefact ~srcs =
  (* XXX We assume here that dynlink is available on the platform.
     If it's not we should build a custom toplevel and invoke that
     instead. *)
  let* b = builder m c dc ~build_dir ~srcs in
  let* target = top_target m dc in
  match target with
  | `Byte -> build_top b ~code:`Byte ~top:"ocaml" ~artefact
  | `Native -> build_top b ~code:`Native ~top:"ocamlnat" ~artefact
  | `Html -> build_html_top b ~top:"jsoo" ~artefact
  | `Node -> fail_unsupported_target m ~outcome:"top" ~target:"node"

let top_action m c dc ~build_dir ~artefact =
  match Brzo_ocaml_conf.target dc with
  | None | Some (`Byte | `Native) -> run_top_cmd m c artefact
  | Some `Html -> exec_html_action m c dc ~build_dir ~artefact
  | Some `Node -> assert false

let top =
  let name = "top" in
  let doc = "Build and load code in the OCaml interactive toplevel." in
  let build_dir_suff = top_build_dir_suff in
  let artefact = top_artefact in
  let build = top_build in
  let action = top_action in
  Brzo_outcome.v
    ~name ~doc ~build_dir_suff ~artefact ~build ~action_has_args:true ~action ()

let utop_build m c dc ~build_dir ~artefact ~srcs =
  let* b = builder m c dc ~build_dir ~srcs in
  match Brzo_ocaml_conf.target dc with
  | None | Some `Byte -> build_top b ~code:`Byte ~top:"utop" ~artefact
  | Some `Native -> fail_unsupported_target m ~outcome:"utop" ~target:"native"
  | Some `Html -> fail_unsupported_target m ~outcome:"utop" ~target:"html"
  | Some `Node -> fail_unsupported_target m ~outcome:"utop" ~target:"node"

let utop =
  let name = "utop" in
  let doc = "Build and load code in the $(b,utop) interactive toplevel." in
  let artefact m _ _ ~build_dir =
    Fut.return Fpath.(build_dir / "utop.cmd")
  in
  let build = utop_build in
  let action = top_action in
  Brzo_outcome.v ~name ~doc ~artefact ~build ~action_has_args:true ~action ()

(* Doc outcome

   XXX Quickly hacked to do the dependency stuff. Also what about generating
       doc for the libs used ? Also there's certainly more to be shared with
       odig here. *)

let assets_dir = "_assets" (* pkg assets directory *)
let docs_dir = "_docs" (* Rendered .md files *)
let doc_dir = "_doc-dir" (* pkg doc-dir directory *)

let odoc_file_for_cobj b pkg cobj =
  let odoc = Fmt.str "%s.odoc" (Fpath.basename ~strip_ext:true cobj) in
  Fpath.(b.build_dir / "odoc" / pkg / odoc)

let odoc_file_for_mld b pkg mld = (* assume mld names are flat *)
  let page = Fmt.str "page-%s.odoc" (Fpath.basename ~strip_ext:true mld) in
  Fpath.(b.build_dir / "odoc" / pkg / page)

let best_doc_cobj b cobj =
  let* () = B0_memo.wait_files b.m [cobj] in
  let exists p = Os.File.exists p |> Log.if_error ~use:false in
  let cmti = Fpath.(cobj -+ ".cmti") in
  if exists cmti then Fut.return cmti else
  let cmt = Fpath.(cobj -+ ".cmt") in
  if exists cmt then Fut.return cmt else
  Fut.return cobj

let cobj_to_odoc b pkg cobj =
  let odoc = odoc_file_for_cobj b pkg cobj in
  let odoc_deps = [] (* FIXME *) in
  begin
    ignore @@
    let* cobj = best_doc_cobj b cobj in
    B0_odoc.Compile.to_odoc b.m ~pkg ~odoc_deps cobj ~o:odoc;
    Fut.return cobj
  end;
  odoc

let mld_to_odoc b pkg pkg_odocs mld =
  let odoc = odoc_file_for_mld b pkg mld in
  let odoc_deps =
    (* XXX odoc compile-deps does not work on .mld files, so we
       simply depend on all of the package's odoc files. This is
       needed for example for {!modules } to work in the index.
       trefis says: In the long term this will be solved since all
       reference resolution will happen at the `html-deps` step. For
       now that seems a good approximation. *)
    pkg_odocs
  in
  B0_odoc.Compile.to_odoc b.m ~pkg ~odoc_deps mld ~o:odoc;
  odoc

let index_mld_for_pkg b pkg_name mld_odocs odocs doc_htmls =
  let index_mld = Fpath.(b.build_dir / "index.mld") in
  begin
    let reads = List.rev_append mld_odocs odocs in
    let stamp = String.concat "" (List.rev_map Fpath.to_string doc_htmls) in
    B0_memo.write b.m ~stamp ~reads index_mld @@
    fun () ->
    let mods = List.rev_map B0_ocaml.Modname.of_path_filename odocs in
    let mods = List.sort String.compare mods in
    let mods = match mods with
    | [] -> ""
    | mods ->
        Fmt.str "\n{1:api API}\n{!modules: %s}" (String.concat " " mods)
    in
    let docs = match mld_odocs, doc_htmls with
    | [], [] -> ""
    | mlds, md_htmls ->
        let mld_link mld =
          let link = Fpath.basename ~strip_ext:true mld in
          let text = String.subrange ~first:5 link in
          Fmt.str "{- {{!%s}%s}}" link (String.Ascii.capitalize text)
        in
        let index = Fpath.(b.build_dir / "html" / "pkg_name" / "index.html") in
        let doc_html_link md_html =
          let url = Fpath.relative ~to_dir:(Fpath.parent index) md_html in
          let url = Fpath.to_url_path url in
          let text = B0_html.El.title_of_filepath (Fpath.to_string md_html) in
          Fmt.str "{- {{:%s}%s}}" url (String.Ascii.capitalize text)
        in
        let mlds = List.rev_map mld_link mlds in
        let htms = List.rev_map doc_html_link md_htmls in
        let links = List.sort String.compare (List.rev_append htms mlds) in
        Fmt.str "\n{1:doc Documentation}\n{ul %s}" (String.concat "\n" links)
    in
    match docs, mods with
    | "", "" ->
        Ok (Fmt.str "{0 %s}\nWrite some [.mli] or [.mld] files !" pkg_name)
    | mlds, mods ->
        Ok (Fmt.str "{0 %s}%s%s" pkg_name mlds mods)
  end;
  index_mld

let mlds_to_odoc b pkg_name odocs mlds doc_htmls =
  let rec loop ~made_index mld_odocs = function
  | mld :: mlds ->
      B0_memo.ready_file b.m mld;
      let mld, made_index = match Fpath.basename mld = "index.mld" with
      | false -> mld, made_index
      | true -> mld, true
      in
      loop ~made_index (mld_to_odoc b pkg_name odocs mld :: mld_odocs) mlds
  | [] when made_index -> List.rev_append mld_odocs odocs
  | [] ->
      let mld = index_mld_for_pkg b pkg_name mld_odocs odocs doc_htmls in
      List.rev_append (mld_to_odoc b pkg_name odocs mld :: mld_odocs) odocs
  in
  loop ~made_index:false [] mlds

let html_deps_resolve b deps = Fut.return []
let pkg_to_html b theme pkg_name cmis ~html_dir md_htmls =
  let mlds = B0_file_exts.(find_files (ext ".mld") b.srcs) in
  let odocs = List.map (cobj_to_odoc b pkg_name) cmis in
  let odoc_files = mlds_to_odoc b pkg_name odocs mlds md_htmls in
  (* mlds_to_odoc already appends odocs
    let odoc_files = List.rev_append odocs mld_odocs in *)
  let deps_file = Fpath.(b.build_dir / pkg_name + ".html.deps") in
  B0_odoc.Html.Dep.write b.m ~odoc_files b.build_dir ~o:deps_file;
  ignore @@
  let* deps = B0_odoc.Html.Dep.read b.m deps_file in
  let* odoc_deps = html_deps_resolve b deps in
  let theme_dir = B0_odoc.Theme.default_uri in
  let theme_uri = match theme with None -> None | Some _ -> Some theme_dir in
  let to_html = B0_odoc.Html.write b.m ?theme_uri ~html_dir ~odoc_deps in
  List.iter to_html odoc_files;
  Fut.return ()


let find_theme b =
  let opam_share () =
    (* FIXME B00_opam *)
    let opam = Cmd.arg "opam" in
    Result.bind (Os.Cmd.get opam) @@ fun opam ->
    Result.bind (Os.Cmd.run_out ~trim:true
                   Cmd.(opam % "var" % "share")) @@ fun share ->
    Fpath.of_string share
  in
  Log.if_error ~level:Log.Warning ~use:None begin
    Result.bind (opam_share ()) @@ fun sharedir ->
    let ts = B0_odoc.Theme.of_dir sharedir in
    let odig_theme =
      let odig t = B0_odoc.Theme.name t = B0_odoc.Theme.odig_default in
      match List.find odig ts with exception Not_found -> None | t -> Some t
    in
    Result.bind (B0_odoc.Theme.get_user_preference ()) @@ function
    | None -> Ok odig_theme
    | Some name ->
        let fallback = match odig_theme with
        | Some t -> Some (B0_odoc.Theme.name t)
        | None -> Some (B0_odoc.Theme.odoc_default)
        in
        Log.if_error' ~level:Log.Warning ~use:odig_theme @@
        Result.bind (B0_odoc.Theme.find ~fallback name ts) @@ fun t ->
        Ok (Some t)
  end

let write_theme m theme ~html_dir = match theme with
| None -> ()
| Some t ->
    let to_dir = Fpath.(html_dir / B0_odoc.Theme.default_uri) in
    B0_odoc.Theme.write m t ~to_dir

let pkg_name c = Fpath.basename ~strip_ext:true (Brzo.Conf.root c)

let md_to_html m ~src_root ~build_dir ~html_dir md =
  let generator = "brzo %%VERSION%% ocaml doc" in
  let o_frag = Fpath.(reroot ~src_root ~dst_root:build_dir md -+ ".htmlf") in
  let o = Fpath.(reroot ~src_root ~dst_root:html_dir  md -+ ".html") in
  B0_memo.ready_file m md;
  B0_cmark.to_html m ~opts:Cmd.empty ~mds:[md] ~generator ~o_frag ~o;
  o

let mds_to_html m c dc ~build_dir ~html_dir ~srcs =
  let* cmark = B0_memo.tool_opt m B0_cmark.tool (* fixme conf lookup *) in
  match cmark with
  | None -> Fut.return []
  | Some _ ->
      let mds = B0_file_exts.(find_files cmark) srcs in
      let src_root = Brzo.Conf.root c in
      let html_dir = Fpath.(html_dir / "_docs") in
      Fut.return @@
      List.rev_map (md_to_html m ~src_root ~build_dir ~html_dir) mds

let build_doc m c dc ~build_dir ~artefact ~srcs =
  let* b = builder m c dc ~build_dir ~srcs in
  let theme = find_theme b and pkg_name = pkg_name c and html_dir = artefact in
  let in_dir = Fpath.(b.build_dir / "cmti") in
  let* () = B0_memo.mkdir m in_dir in
  let opts = Cmd.arg "-g" in
  let* local_mods = local_mods b ~opts ~build_dir:in_dir ~mli_only:true in
  let comp = comp_of_code `Native in (* FIXME do something smart *)
  let cmis = compile_intfs b ~comp ~opts ~local_mods ~build_dir:in_dir in
  write_merlin_file b;
  let* () = B0_memo.mkdir m html_dir in
  let* md_htmls = mds_to_html m c dc ~build_dir ~html_dir ~srcs in
  pkg_to_html b theme pkg_name cmis ~html_dir md_htmls;
  let without_theme = match theme with None -> false | Some _ -> true in
  let build_dir = b.build_dir in
  B0_odoc.Support_files.write b.m ~without_theme ~html_dir ~build_dir;
  write_theme b.m theme ~html_dir;
  Fut.return ()

let doc =
  let name = "doc" in
  let doc = "Build and show source API documentation and manuals." in
  let artefact m c _ ~build_dir = Fut.return (Fpath.(build_dir / "html")) in
  let build = build_doc in
  let action dc c b ~build_dir ~artefact =
    let artefact = Fpath.(artefact / pkg_name c) in
    Brzo_outcome.Action.show_uri dc c b ~build_dir ~artefact
  in
  Brzo_outcome.v ~name ~doc ~artefact ~build ~action_has_args:false ~action ()

(* Outcomes *)

let outcomes = [doc; exec; top; utop]
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
