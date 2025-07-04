(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Brzo's [default] command. *)

val term : B0_std.Os.Exit.t Cmdliner.Term.t
(** [term] is the term for the [default] command. *)

val cmd : B0_std.Os.Exit.t Cmdliner.Cmd.t
(** [cmd] is the command line for [default]. *)
