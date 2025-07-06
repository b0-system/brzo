(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Ties the configuration knot.

    Configuration in {!Brzo.Conf} needs the {!Brzo_domain_list} which
    uses {!Brzo_domain} which uses {!Brzo.Conf}. This module allows to
    break the cyclic dependency by transforming {!Brzo_domain}s to
    {!Brzo.Pre_domain}s. *)

open B0_std

(** {1:conf Configuration} *)

val for_domain : domain:Brzo_domain.t -> Brzo.Conf.t Cmdliner.Term.t
(** [conf_for_domain ~domain] is a cmdliner term for configuration
    for domain [domain]. The root is determined and the BRZO file is
    consulted. *)

val use_brzo_file : Brzo.Conf.t Cmdliner.Term.t
(** [use_brzo_file] is a cmdliner term for brzo configuration when no
    domain outcome is involved. The root is determined and the BRZO file
    is consulted. *)

val no_brzo_file : Brzo.Conf.t Cmdliner.Term.t
(** [no_brzo_file] is a cmdliner term for brzo configuration
    when no domain outcome is involved. The root is determined but the
    BRZO file is not consulted. *)

val auto_cwd_root_and_no_brzo_file : Brzo.Conf.t Cmdliner.Term.t
(** [auto_root_root_no_brzo_file] is a cmdliner term for brzo configuration
    when no domain outcome is involved. The root falls back to cwd
    if it can be determined and the BRZO file is not consulted. *)
