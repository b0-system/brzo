(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Brzo OCaml domain configuration. *)

open B0_std

(** {1 OCaml configuration} *)

include Brzo.Pre_domain.CONF

type target = [ `Byte | `Html | `Native | `Node ]
(** The type for OCaml compilation targets. *)

val target_to_string : target -> string
(** [target_to_string t] is [t] as a string token. *)

val default_target :
  default_native_if:B0_memo.Tool.t -> B0_memo.t -> target option ->
  target Fut.t

val target : t -> target option
val ocamlpath : t -> Fpath.t list
val libs : t -> Fpath.t list
val lock_libs : t -> bool
