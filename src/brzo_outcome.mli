(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Outcomes *)

open B0_std

(** {1:steps Outcome steps} *)

type 'a build_dir_suff =
  B0_memo.t -> Brzo.Conf.t -> 'a -> string Fut.t
(** The type for build directory suffix determination. [build_dir_suff
    m c dc] is a suffix added to the build directory name. It can be
    used if your outcome has multiple targets (see e.g. the OCaml
    domain) and you would like to be able to have them live in
    parallel in the build dir. *)

type 'a artefact =
  B0_memo.t -> Brzo.Conf.t -> 'a -> build_dir:Fpath.t -> Fpath.t Fut.t
(** The type for outcome artefact path determination. [artefact m c dc
    ~build_dir] is the absolute path, contained in the domain specific
    [build_dir] to the outcome artefact (can be a directory). *)

type 'a build =
  B0_memo.t -> Brzo.Conf.t -> 'a -> build_dir:Fpath.t ->
  artefact:Fpath.t -> srcs:B0_file_exts.map -> unit Fut.t
(** The type for outcome artefact constructions. [build m c dc
    ~build_dir ~artefact ~srcs] builds [artefact] from [srcs] using
    [m]. [build_dir] is clean. [artefact] is the result of the {!artefact}
    function. *)

type 'a action =
  B0_memo.t -> Brzo.Conf.t -> 'a -> build_dir:Fpath.t -> artefact:Fpath.t ->
  (unit -> (Os.Exit.t, string) result) Fut.t
(** The type for outcome action. [action m c dc ~artefact] returns the
    action on [artefact] (already tested for existence and constructed
    by the outcome's {!artefact} function). When the function is
    called it is made sure that build was peformed, finished, and that
    it didn't error. *)

(** {1:outcomes Outcomes} *)

type 'a t
(** The type for outcomes with domain specific configuration of type ['a]. *)

val v :
  name:string -> doc:string -> ?build_dir_suff:'a build_dir_suff ->
  artefact:'a artefact -> build:'a build -> action_has_args:bool ->
  action:'a action -> unit -> 'a t
(** [v] defines an outcome. See the corresponding accessors for the
    semantics of arguments. [build_dir_suff] defaults to
    [fun _ _ _ k -> k ""]. *)

val name : 'a t -> string
(** [name o] is the outcome name. Used in particular to define the
    command line option to select the outcome. *)

val build_dir_suff : 'a t -> 'a build_dir_suff
(** [build_dir_suff o] is [o]'s build directory suffix function. *)

val artefact : 'a t -> 'a artefact
(** [artefact o] is [o]'s artefact determination function. *)

val build : 'a t -> 'a build
(** [build o] is [o]'s build function. *)

val action_has_args : 'a t -> bool
(** [action_has_args o] is [true] iff only the outcome action
    supports cli args. *)

val action : 'a t -> 'a action
(** [action o] is [o]'s action function. *)

val pre_outcome : 'a t -> Brzo.Pre_domain.outcome
(** [pre_outcome o] is [o] as a pre-outcome. *)

val with_outcome : name:string -> doc:string -> 'a t -> 'a t
(** [with_outcome ~name ~doc o] is [o] with name and doc string
    respectively changed to [name] and [doc]. *)

val get : string -> 'a t list -> 'a t
(** [get n os] is the outcome of [os] with name [n]. *)

(** {1:predefined_actions Predefined actions} *)

(** Predefined actions. *)
module Action : sig
  val exec : 'a action
  (** [exec _ c _ _ ~artefact] executes program [artefact] using
      {!Brzo.Conf.action_args} and {!Brzo.Conf.cwd}. Normally the function
      does not return. *)

  val show_pdf : 'a action
  (** [show_pdf _ c _ _ ~artefact] shows path [artefact] in a PDF
      viewer using {!Brzo.Conf.pdf_viewer}. *)

  val show_uri : 'a action
  (** [show_uri _ c _ _ ~artefact] shows path [artefact] in a browser
        using {!Brzo.Conf.browser}. *)
end

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
