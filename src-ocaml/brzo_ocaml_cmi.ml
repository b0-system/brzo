(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Fut.Syntax
open B00
open B00_ocaml

type t =
  { file : Fpath.t;
    mod_ref : Mod_ref.t;
    mod_names : Mod_name.Set.t;
    deps : Mod_ref.Set.t }

let read m file =
  let* () = Memo.wait_files m [file] in
  let name, digest, mod_names, deps =
    Brzo_read_cmi.read file |> Memo.fail_if_error m
  in
  let mod_ref = Mod_ref.v name digest in
  let add_dep acc (n, d) = Mod_ref.Set.add (Mod_ref.v n d) acc in
  let deps = List.fold_left add_dep Mod_ref.Set.empty deps in
  Fut.return { file; mod_ref; mod_names; deps }

let file cmi = cmi.file
let mod_ref cmi = cmi.mod_ref
let mod_names cmi = cmi.mod_names
let deps cmi = cmi.deps
let pp ppf cmi =
  Fmt.pf ppf "%a %a" Mod_ref.pp cmi.mod_ref Fpath.pp_quoted cmi.file

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
