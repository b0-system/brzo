(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Cmdliner

let all_domains = List.map Brzo_domain.pre_domain Brzo_domain_list.v

let for_domain ~domain =
  let domain = Some (Brzo_domain.pre_domain domain) in
  Brzo.Cli.conf ~auto_cwd_root:false ~use_brzo_file:true ~domain ~all_domains

let domain = None

let use_brzo_file =
  Brzo.Cli.conf ~auto_cwd_root:false ~use_brzo_file:true ~domain ~all_domains

let no_brzo_file =
  Brzo.Cli.conf ~auto_cwd_root:false ~use_brzo_file:false ~domain ~all_domains

let auto_cwd_root_and_no_brzo_file =
  Brzo.Cli.conf ~auto_cwd_root:true ~use_brzo_file:false ~domain ~all_domains
