(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
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
