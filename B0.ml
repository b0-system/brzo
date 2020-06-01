open B0_kit.V000
open B00_std
open B00_std.Fut.Syntax

(* OCaml library names *)

let cmdliner = B0_ocaml.lib "cmdliner"
let b0_b00_std = B0_ocaml.lib "b0.b00.std"
let b0_b00 = B0_ocaml.lib "b0.b00"
let b0_b00_kit = B0_ocaml.lib "b0.b00.kit"
let compiler_libs_common = B0_ocaml.lib "compiler-libs.common"

(* Units *)

let brzo_tool =
  let requires =
    [cmdliner; b0_b00_std; b0_b00; b0_b00_kit; compiler_libs_common]
  in
  let ocaml_cond b =
    let* version = B0_ocaml.version b in
    let dir = match version with
    | v when v < (3, 08, 0, None) -> "brzo_read_cmi_pre_408"
    | _ -> "brzo_read_cmi_geq_408"
    in
    let root_dir = B0_build.current_root_dir b in
    let file = Fpath.(root_dir / "src-ocaml" / dir / "brzo_read_cmi.ml") in
    B00.Memo.file_ready (B0_build.memo b) file;
    Fut.return (Fpath.Set.singleton file)
  in
  let srcs =
    [`Dir "src"; `Dir "src-c"; `Dir "src-cmark"; `Dir "src-latex";
     `Dir "src-ocaml"; `Fut ocaml_cond]
  in
  B0_ocaml.Unit.exe "brzo" ~doc:"brzo tool" ~requires ~srcs

(* Packs *)

let default =
  let units = B0_unit.list () in
  let meta = B0_meta.v @@ B0_meta.[
      authors, ["The brzo programmers"];
      maintainers, ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"];
      homepage, "https://erratique.ch/software/brzo";
      online_doc, "https://erratique.ch/software/brzo/doc";
      doc_tags, ["build"; "dev"; "org:erratique"; "org:b0-system"];
      licenses, ["ISC"];
      repo, "git+https://erratique.ch/repos/brzo.git";
      issues, "https://github.com/b0-system/brzo/issues"; ]
  in
  B0_pack.v "default" ~doc:"The brzo project" ~meta ~locked:true units
