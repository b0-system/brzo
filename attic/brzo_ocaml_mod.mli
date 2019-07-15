(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std

(* XXX We should move to this model on which best-effort
   name resolution can be peformed.

type mod_path = string
type import =
| Open of mod_path * import list
| Using of String.Set.t

type imports = import list
type exports = String.Set.t
type mod_sig = { imports : imports; exports : exports }
type mod_impl = { imports : imports; }

val mod_impl_of_ml : string -> mod_impl
val mod_sig_of_mli : string -> mod_sig
*)
type path = string list

val ml_deps : Fpath.t -> ((bool * string) list, string) result
val mli_deps : Fpath.t -> ((bool * string) list, string) result
val cmi_defs : Fpath.t -> (path list, string) result

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
