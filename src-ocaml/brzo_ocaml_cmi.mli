(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Compiled interfaces. *)

open B00_std
open B00_ocaml

type t
(** The type for compiled interfaces. *)

val read : B00.Memo.t -> Fpath.t -> t Fut.t
(** [read f] reads an object file from [f]. *)

val file : t -> Fpath.t
(** [file cmi] is the file path of [cmi]. *)

val mod_ref : t -> Mod.Ref.t
(** [mod_ref cmi] is the module reference of [cmi]. *)

val deps : t -> Mod.Ref.Set.t
(** [deps cmi] is the set of modules interfaces imported by [cmi]. *)

val mod_names : t -> Mod.Name.Set.t
(** [mod_names cmi] are the unqualified module names defined by
        [cmi] (including its name). Sligthly wrong because stops at module
        aliases, these are not resolved to further cmis. *)

val pp : t Fmt.t
(** [pp] formats a compiled interface. *)

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
