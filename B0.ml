open B0_kit.V000
open B00_std

(* OCaml library names *)

let cmdliner = B0_ocaml.lib "cmdliner/cmdliner"
let b00_std = B0_ocaml.lib "b0.b00.std/b00_std"
let b00 = B0_ocaml.lib "b0.b00/b00"
let b00_kit = B0_ocaml.lib "b0.b00.kit/b00_kit"
let compiler_libs_common = B0_ocaml.lib "ocaml.compiler-libs.common"

(* Units *)

let brzo_tool =
  let requires = [cmdliner; b00_std; b00; b00_kit; compiler_libs_common] in
  let ocaml_cond b k =
    B00.Store.get (B0_build.store b) B00_ocaml.Conf.key  @@ fun conf ->
    let src = match B00_ocaml.Conf.version conf with
    | v when v < (3, 08, 0, None) -> "brzo_read_cmi_pre_408/brzo_read_cmi.ml"
    | _ -> "brzo_read_cmi_geq_408/brzo_read_cmi.ml"
    in
    k (Fpath.Set.singleton (Fpath.v src))
  in
  let srcs =
    [ `D "src"; `D "src-c"; `D "src-cmark"; `D "src-latex";
      `D "src-ocaml"; `Fiber ocaml_cond ]
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
  B0_pack.v "default" ~doc:"brzo tool" ~meta ~locked:true units
