(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Compiled interfaces. *)

open B0_std

type t
(** The type for compiled interfaces. *)

val read : B0_memo.t -> Fpath.t -> t Fut.t
(** [read f] reads an object file from [f]. *)

val file : t -> Fpath.t
(** [file cmi] is the file path of [cmi]. *)

val modref : t -> B0_ocaml.Modref.t
(** [modref cmi] is the module reference of [cmi]. *)

val deps : t -> B0_ocaml.Modref.Set.t
(** [deps cmi] is the set of modules interfaces imported by [cmi]. *)

val modnames : t -> B0_ocaml.Modname.Set.t
(** [modnames cmi] are the unqualified module names defined by
    [cmi] (including its name). Sligthly wrong because stops at module
    aliases, these are not resolved to further cmis. *)

val pp : t Fmt.t
(** [pp] formats a compiled interface. *)
