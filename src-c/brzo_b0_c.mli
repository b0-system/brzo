(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** C B0 helpers. *)

open B0_std

module Tool : sig
  val gcc : B0_memo.Tool.t
  (** [gcc] is the [gcc] tool. *)
end

module Conf : sig
  val obj_ext : B0_memo.t -> Fpath.ext Fut.t
end

(** C include dependencies (via [gcc] [-M])

    Parses the format output by [-M] which is basically a Makefile
    rule. *)
module Inc_deps : sig

  val write :
    ?deps:Fpath.t list -> B0_memo.t -> src:Fpath.t -> o:Fpath.t -> unit
  (** [write m ~deps src o] writes dependencies of [src] in file
      [o]. [deps] indicates these files should become ready first: we
      do not use [-MG] as it leads to output race conditions so one
      needs to make sure all generated includes that may end up being
      used {!src} are generated before asking for dependencies. *)

  val read : B0_memo.t -> src:Fpath.t -> Fpath.t -> Fpath.t list Fut.t
  (** [read m file] reads dependencies produced by {!write} in [file]
      for source [src] (whose parent directory is used to make paths
      absolute, see {!of_string}). *)

  (** {1:low Low-level parsing} *)

  val of_string :
    ?file:Fpath.t -> root:Fpath.t -> string -> (Fpath.t list, string) result
  (** [of_string ~file ~src s] parses [-M] dependencies data from [s]
      assuming it was read from file [file] (for error reporting,
      defaults to {!Os.File.dash}). [root] is used to make file paths
      absolute (whether file paths end up being absolute or not
      seem to depend on the cwd when {!write} is invoked. *)
end

module Compile : sig
  val c_to_o :
    ?post_exec:(B0_zero.Op.t -> unit) -> ?k:(int -> unit) ->
    ?args:B0_std.Cmd.t -> B0_memo.t -> deps:Fpath.t list -> c:Fpath.t ->
    o:Fpath.t -> unit
  (** [c_to_o m ~deps ~c ~o] compiles [c] to the object file [o]
      assuming [c] depends on [deps]. *)
end

module Link : sig

  val exe :
    ?post_exec:(B0_zero.Op.t -> unit) -> ?k:(int -> unit) ->
    ?args:B0_std.Cmd.t -> B0_memo.t -> objs:Fpath.t list -> o:Fpath.t ->
    unit
    (** [exe m ~args ~objs ~o] links the objects [objs] into executable [o]. *)
end



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
