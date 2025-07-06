(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Fut.Syntax

type t =
  { file : Fpath.t;
    modref : B0_ocaml.Modref.t;
    modnames : B0_ocaml.Modname.Set.t;
    deps : B0_ocaml.Modref.Set.t }

let read m file =
  let* () = B0_memo.wait_files m [file] in
  let name, digest, modnames, deps =
    Brzo_read_cmi.read file |> B0_memo.fail_if_error m
  in
  let modref = B0_ocaml.Modref.make name digest in
  let add_dep acc (n, d) =
    B0_ocaml.Modref.Set.add (B0_ocaml.Modref.make n d) acc
  in
  let deps = List.fold_left add_dep B0_ocaml.Modref.Set.empty deps in
  Fut.return { file; modref; modnames; deps }

let file cmi = cmi.file
let modref cmi = cmi.modref
let modnames cmi = cmi.modnames
let deps cmi = cmi.deps
let pp ppf cmi =
  Fmt.pf ppf "%a %a" B0_ocaml.Modref.pp cmi.modref Fpath.pp_quoted cmi.file
