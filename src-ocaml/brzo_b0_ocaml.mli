(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** B0 [ocaml] support. *)

open B0_std

(** Data-driven OCaml module and compilation objects resolver.

    {b FIXME.} The stdlib is difficult to specify for opam system
    switches. E.g. if one wants to use [`Locked]. In a regular
    opam switch the dependency restriction is [ocaml], in
    a system switch it can't be specified. *)
module Mod_resolver : sig

  (** {1:res Resolution restrictions} *)

  type dep = Fpath.t
  (** The type for dependency restrictions. This is a relative file
      path denoting a directory in one of the dependency directories. *)

  type deps = [ `Open of dep list | `Locked of dep list ]
  (** The type for dependency restrictions. Restrictions are
      expressed relative to dependency directories [ds].
      {ul
      {- [`Open ps], ambiguities are resolved by the first matching
         path of [ps] in the first [ds] (if any).}
      {- [`Locked ps], dependencies are only resolved in [ps] directories
         of [ds] and in case of ambiguity in the first matching path of
         [ps] in the first [ds] (if any).}}

      {b Note.} [`Open] restrictions make compilation successes
      dependent on the state of the dependency
      directories. Technically so does [`Locked], but it shouldn't
      assuming an opam like package install structure. *)

  val unrestricted : deps
  (** [unrestricted] is [`Open []], unrestricted dependencies. *)

  (** {1:resolve Resolver} *)

  type t
  (** The type for data-driven module resolvers. *)

  val create :
    B0_memo.t -> memo_dir:Fpath.t -> dep_dirs:Fpath.t list -> deps ->
    t * [`Miss_deps of dep list]
  (** [create m ~memo_dir ~dep_dirs deps] is a module resolver with
      restrictions [deps] in dependency directories [dep_dirs] using
      [m] and [memo_dir] to memoize its results. [`Miss_dep] is the
      list of dependencies that couldn't be found in either of the
      [dep_dirs]. *)

  val memo : t -> B0_memo.t
  (** [memo r] is the memoizer of [r]. *)

  val index : t -> B0_findex.t
  (** [index r] is the file index of [r], that is the {e unrestricted} index
      of [dep_dirs]. *)

  val dep_dirs : t -> Fpath.t list
  (** [dep_dirs r] are the dependency directories. *)

  val deps : t -> deps
  (** [deps r] are the dependency restrictions. *)

  val dep_dirs_deps : t -> dep list
  (** [dep_dir_deps r] are all the dependency restrictions that
      can be found in [r]; regardless of {!deps}. *)

  val dep_of_file : t -> Fpath.t -> dep option
  (** [find_dep r f] is the dependency restriction for [file] in [r]. This
      is never [None] if [f] is in {!index}. *)

  (** {1:lookups Lookups}

      {b Note.} File objects returned by [find_*] functions are made ready
      in [memo r]. *)

  val cmi_obj : t -> Fpath.t -> Brzo_ocaml_cmi.t Fut.t
  (** [cmi_obj r file] asks for the cmi compilation object of file [file].
      FIXME let the fut be a fiber. *)

  val find_cmi_side_cmx_file : t -> Brzo_ocaml_cmi.t -> Fpath.t option
  (** [find_cmi_side_cmx_file r cmi] returns an existing side [cmx] file for
      [cmi] located along side it. FIXME be principled about that:
      makes as a side effect ready in [memo r]. *)

  val find_cmi_files_for_modname : t -> B0_ocaml.Modname.t -> Fpath.t list
  (** [find_cmi_files_for_modname r mn] finds cmi files whose filename could
      resolve to module name [mn]. *)

  val find_cmis_for_modname :
    t -> B0_ocaml.Modname.t -> Brzo_ocaml_cmi.t list Fut.t
  (** [find_cmis_for_modname r mn] finds cmi information for files
      whose filename could resolve to module name [mn] in [r]. *)

  val find_cmis_for_modref :
    t -> B0_ocaml.Modref.t -> Brzo_ocaml_cmi.t list Fut.t
  (** [find_cmis_for_modref r mref] are the cmis that resolve to [mref]
      in [r]. *)

  val find_impl_for_modref :
    t -> ext:string -> B0_ocaml.Modref.t -> B0_ocaml.Cobj.t option Fut.t
  (** [find_impl_for_modref r ~ext mref] continues with an
      implementation file for [mref] with extension [ext] and a
      suitable cmi file to use. *)

  val find_rec_impls_for_modrefs :
    ?deps:(B0_ocaml.Cobj.t -> B0_ocaml.Modref.Set.t) -> t -> ext:string ->
    B0_ocaml.Modref.Set.t -> B0_ocaml.Cobj.t list Fut.t
  (** [find_rec_impls_for_modrefs ~deps r ~ext mrefs] continues with a list
      of implementation files with extentions [ext] for [mref]s and
      their recursive dependencies determined according to [deps]
      (defaults to {!Cobj.link_deps}). The list is sorted in link
      order. *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers

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
