#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let copy src dst =
  Log.info (fun m -> m "cp %S %S" src dst);
  OS.File.read src >>= fun content ->
  let content = strf "# 1 %S\n%s" src content in
  OS.File.write dst content

let ocaml_conditional c =
  let maj, min, _, _ = Conf.OCaml.version (Conf.OCaml.v c `Host_os) in
  let dst = "src/ocaml/brzo_read_cmi.ml" in
  match (maj, min) < (4,08) with
  | true  -> copy "src/ocaml/brzo_read_cmi_pre_408/brzo_read_cmi.ml" dst
  | false -> copy "src/ocaml/brzo_read_cmi_geq_408/brzo_read_cmi.ml" dst

let () =
  let build = Pkg.build ~pre:ocaml_conditional () in
  Pkg.describe ~build "brzo" @@ fun c ->
  Ok [ Pkg.mllib "src/brzo.mllib";
       Pkg.bin "src/tool/brzo_main" ~dst:"brzo";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/manual.mld" ~dst:"odoc-pages/manual.mld"; ]
