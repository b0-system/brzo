(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

val read :
  Fpath.t ->
  (string * Digest.t * String.Set.t * (string * Digest.t) list, string) result
(** [read cmi] is [Ok (name, digest, names, deps)] with
    {ul
    {- [name] the module name of the interface.}
    {- [digest] the digest of the inteface.}
    {- [names] are the unqualified module names defined in the interface,
       including [name] itself. This is slightly incomplete because it
       stops at module aliases: these would need to be resolved to
       further cmis to find the names therein.}
    {- [deps] are the digested module interfaces imported by this
       interface.}} *)
