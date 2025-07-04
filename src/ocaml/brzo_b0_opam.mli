(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Opam b0 helpers. *)

open B0_std

val tool : B0_memo.Tool.t
(** [tool] is the [opam] tool. *)

val exists : B0_memo.t -> bool Fut.t
val if_exists : B0_memo.t -> (unit -> 'a Fut.t) -> 'a option Fut.t
val lib_dir : B0_memo.t -> ?switch:string -> unit -> Fpath.t Fut.t

val list :
  B0_memo.t -> ?switch:string -> [ `Available | `Installed ] -> unit ->
  string list Fut.t

type pkg = string * string option

val pkg_list : ?switch:string -> unit -> (pkg list, string) result
