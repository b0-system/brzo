(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers

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
