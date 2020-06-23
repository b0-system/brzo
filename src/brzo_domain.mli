(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Brzo domains. *)

open B00_std

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

val fingerprint : t -> B00_fexts.t
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
