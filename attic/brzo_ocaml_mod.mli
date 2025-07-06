(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

(* XXX We should move to this model on which best-effort
   name resolution can be peformed.

type mod_path = string
type import =
| Open of mod_path * import list
| Using of String.Set.t

type imports = import list
type exports = String.Set.t
type mod_sig = { imports : imports; exports : exports }
type mod_impl = { imports : imports; }

val mod_impl_of_ml : string -> mod_impl
val mod_sig_of_mli : string -> mod_sig
*)
type path = string list

val ml_deps : Fpath.t -> ((bool * string) list, string) result
val mli_deps : Fpath.t -> ((bool * string) list, string) result
val cmi_defs : Fpath.t -> (path list, string) result
