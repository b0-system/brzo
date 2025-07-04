(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** C B0 helpers. *)

open B0_std

module Tool : sig
  val doxygen : B0_memo.Tool.t
  (** [dogygen] is the [doxygen] tool. *)

  val dot : B0_memo.Tool.t
  (** [dot] is the dot tool. *)
end


val cmd : reads:Fpath.t list -> conf:string -> unit
