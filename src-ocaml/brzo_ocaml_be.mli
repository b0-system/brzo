(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** OCaml domain best-effort helpers. *)

(** {1:resolver Resolver helpers} *)

open B0_std
open B00
open B00_ocaml
open Brzo_b0_ocaml

type ambs = [ `Ambs of (Mod.Name.t * Fpath.t list) list ]
(** The type for ambiguous external resolutions. The module name
    and the list of files that could match. *)

val resolve_intf_deps :
  Mod_resolver.t -> local_mods:Mod.Src.t Mod.Name.Map.t -> in_dir:Fpath.t ->
  Mod.Name.Set.t ->
  (Fpath.t list * Fpath.t list * Mod.Name.Set.t * ambs) Fut.t
(** [resolve_intf_deps] is like {!resolve_impl_deps} but for compiling
    and interface. *)

val resolve_impl_deps :
  Mod_resolver.t -> code:Conf.code -> local_mods:Mod.Src.t Mod.Name.Map.t ->
  in_dir:Fpath.t -> Mod.Name.Set.t ->
  (Fpath.t list * Fpath.t list * Mod.Name.Set.t * ambs) Fut.t
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
  Mod_resolver.t -> Fpath.t -> unresolved:Mod.Name.Set.t -> ambs -> unit Fut.t
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

val suggest_pkgs_for_mod_name : Mod.Name.t ->
  pkgs:(string * string option) list ->
  [ `Fuzzy_prefix_match of string list | `Prefix_match of string | `None ]
(** [suggest_package_for_mod_name m pkgs] does basically what it says. *)

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
