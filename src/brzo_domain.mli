(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Brzo domains. *)

open B0_std

(** {1:domain Domains} *)

(** Signature implemented by domains *)
module type T = sig
  include Brzo.Pre_domain.T

  val outcomes : Conf.t Brzo_outcome.t list
  (** [outcome] is the domain's list of outcomes. *)
end

type t = V : (module T with type Conf.t = 'a) -> t
(** The type for domains. *)

val name : t -> string
(** [name d] is the identifier of domain [d]. *)

val fingerprint : t -> B0_file_exts.t
(** [fingerprint d] is the fingerprint of [d]. *)

val pre_domain : t -> Brzo.Pre_domain.t
(** [pre_domain d] is the pre-domain of [d]. *)

val run : Brzo.Conf.t -> t -> Os.Exit.t
(** [run c d] runs domain [d] with configuration [c]. *)

(** {1:find Finding domains} *)

val find : string -> t list -> (t, string) result
(** [find name ds] is the domain identified named [n] in [ds]. *)

val of_conf : Brzo.Conf.t -> t list -> (t, string) result
(** [of_conf c ds] is the domain from [ds] selected in configuration [c]. *)
