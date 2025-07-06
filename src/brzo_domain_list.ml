(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)


let v : Brzo_domain.t list =
  (* Ordered by fingerprint priority. *)
  [ Brzo_domain.V (module Brzo_ocaml);
    Brzo_domain.V (module Brzo_c);
    Brzo_domain.V (module Brzo_latex);
    Brzo_domain.V (module Brzo_cmark); ]
