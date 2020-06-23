open B0_kit.V000
open B00_std
open B00_std.Fut.Syntax

(* OCaml library names *)

let cmdliner = B0_ocaml.libname "cmdliner"
let b0_b00_std = B0_ocaml.libname "b0.b00.std"
let b0_b00 = B0_ocaml.libname "b0.b00"
let b0_b00_kit = B0_ocaml.libname "b0.b00.kit"
let compiler_libs_common = B0_ocaml.libname "compiler-libs.common"

(* Units *)

let brzo_tool =
  let requires =
    [cmdliner; b0_b00_std; b0_b00; b0_b00_kit; compiler_libs_common]
  in
  let ocaml_cond b =
    (* XXX for this to work we need a corresponding the mli (whatever
       the content) in the directories with the
       implementation. Toolchain idiosyncrasies strike again
       https://github.com/ocaml/ocaml/issues/9717 *)
    let* version = B0_ocaml.version b in
    let dir = match version with
    | v when v < (3, 08, 0, None) -> "brzo_read_cmi_pre_408"
    | _ -> "brzo_read_cmi_geq_408"
    in
    let scope_dir = B0_build.current_scope_dir b in
    let file = Fpath.(scope_dir / "src-ocaml" / dir / "brzo_read_cmi.ml") in
    B00.Memo.file_ready (B0_build.memo b) file;
    Fut.return (Fpath.Set.singleton file)
  in
  let srcs =
    Fpath.[`Dir (v "src"); `Dir (v "src-c");
           `Dir (v "src-cmark"); `Dir (v "src-latex");
           `Dir (v "src-ocaml"); `Fut ocaml_cond]
  in
  B0_ocaml.exe "brzo" ~doc:"brzo tool" ~requires ~srcs

(* Packs *)

let default =
  let units = B0_unit.list () in
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The brzo programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/brzo"
    |> add online_doc "https://erratique.ch/software/brzo/doc"
    |> add description_tags ["build"; "dev"; "org:erratique"; "org:b0-system"]
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/brzo.git"
    |> add issues "https://github.com/b0-system/brzo/issues"
    |> tag B0_opam.tag
  in
  B0_pack.v "default" ~doc:"The brzo project" ~meta ~locked:true units
