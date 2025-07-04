(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** OCaml domain best-effort helpers. *)

(** {1:resolver Resolver helpers} *)

open B0_std
open Brzo_b0_ocaml

type ambs = [ `Ambs of (B0_ocaml.Modname.t * Fpath.t list) list ]
(** The type for ambiguous external resolutions. The module name
    and the list of files that could match. *)

val resolve_intf_deps :
  Mod_resolver.t -> local_mods:B0_ocaml.Modsrc.t B0_ocaml.Modname.Map.t ->
  in_dir:Fpath.t -> B0_ocaml.Modname.Set.t ->
  (Fpath.t list * Fpath.t list * B0_ocaml.Modname.Set.t * ambs) Fut.t
(** [resolve_intf_deps] is like {!resolve_impl_deps} but for compiling
    and interface. *)

val resolve_impl_deps :
  Mod_resolver.t -> code:B0_ocaml.Code.t ->
  local_mods:B0_ocaml.Modsrc.t B0_ocaml.Modname.Map.t ->
  in_dir:Fpath.t -> B0_ocaml.Modname.Set.t ->
  (Fpath.t list * Fpath.t list * B0_ocaml.Modname.Set.t * ambs) Fut.t
(** [resolve_comp_deps r ~code ~local_mods ~in_dir deps] resolve
    [deps] for compiling an implementation to [code] assuming local
    module [local_mods] are compiled in [in_dir]. This results in
    [local_objs, ext_objs, unresolved, amb] with:
    {ul
    {- [local_deps] are path to compilation objects that corresponds to
       [local_mods].}
    {- [ext_objs] are external compilatino objects as resolved by [r]
       and its constraints.}
    {- [unresolved] are the elements of [deps] that couldn't be resolved.}
    {- [amb] are the external resolutions that were ambiguous.}} *)

val handle_amb_deps :
  Mod_resolver.t -> Fpath.t -> unresolved:B0_ocaml.Modname.Set.t -> ambs ->
  unit Fut.t
(** [handle_amb_deps file ~unresolved ambs] continues if [ambs] is
    empty and otherwise fails the fiber (* FIXME *) with help on how to restrict
    the resolver or getting unresolved dependencies. [file] is the
    file for which amibguities were reported. *)

val handle_miss_user_deps :
  Mod_resolver.t -> [`Miss_deps of Mod_resolver.dep list ] -> unit Fut.t
(** [handle_miss_user_deps r miss k] continues with [k] if [miss] is empty
    and otherwise fails the Fut.t with help on how to try to get the
    missing dependencies. *)

(** {1:suggest Suggesting} *)

val suggest_pkgs_for_modname : B0_ocaml.Modname.t ->
  pkgs:(string * string option) list ->
  [ `Fuzzy_prefix_match of string list | `Prefix_match of string | `None ]
(** [suggest_package_for_mod_name m pkgs] does basically what it says. *)
