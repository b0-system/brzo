open B0_kit.V000
open Fut.Syntax

(* OCaml library names *)

let cmdliner = B0_ocaml.libname "cmdliner"
let b0_std = B0_ocaml.libname "b0.std"
let b0_memo = B0_ocaml.libname "b0.memo"
let b0_file = B0_ocaml.libname "b0.file"
let b0_kit = B0_ocaml.libname "b0.kit"
let compiler_libs_common = B0_ocaml.libname "compiler-libs.common"

(* Units *)

let brzo_tool =
  let ocaml_cond b =
    (* XXX for this to work we need a corresponding the mli (whatever
       the content) in the directories with the
       implementation. Toolchain idiosyncrasies strike again
       https://github.com/ocaml/ocaml/issues/9717 *)
    let* version = B0_ocaml.Conf.version' b in
    let dir = match version with
    | v when v < (3, 08, 0, None) -> "brzo_read_cmi_pre_408"
    | _ -> "brzo_read_cmi_geq_408"
    in
    let scope_dir = B0_build.current_scope_dir b in
    let file = Fpath.(scope_dir / "src-ocaml" / dir / "brzo_read_cmi.ml") in
    B0_memo.file_ready (B0_build.memo b) file;
    Fut.return (Fpath.Set.singleton file)
  in
  let requires =
    [cmdliner; b0_std; b0_memo; b0_file; b0_kit; compiler_libs_common]
  in
  let srcs =
    [`Dir ~/"src"; `Dir ~/"src-c";
     `Dir ~/"src-cmark"; `Dir ~/"src-latex";
     `Dir ~/"src-ocaml"; `Fut ocaml_cond]
  in
  B0_ocaml.exe "brzo" ~public:true ~doc:"brzo tool" ~requires ~srcs

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> B0_meta.(add authors) ["The brzo programmers"]
    |> B0_meta.(add maintainers)
       ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> B0_meta.(add homepage) "https://erratique.ch/software/brzo"
    |> B0_meta.(add online_doc) "https://erratique.ch/software/brzo/doc"
    |> B0_meta.(add description_tags)
       ["build"; "dev"; "org:erratique"; "org:b0-system"]
    |> B0_meta.(add licenses) ["ISC"]
    |> B0_meta.(add repo) "git+https://erratique.ch/repos/brzo.git"
    |> B0_meta.(add issues) "https://github.com/b0-system/brzo/issues"
    |> B0_meta.tag B0_opam.tag
  in
  B0_pack.make "default" ~doc:"The brzo project" ~meta ~locked:true @@
  B0_unit.list ()
