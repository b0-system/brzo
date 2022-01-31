(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

let domain c =
  Log.if_error ~use:Brzo.Exit.undefined_domain @@
  Result.bind (Brzo_domain.of_conf c Brzo_domain_list.v) @@
  fun d -> Log.app (fun m -> m "@[%s@]" (Brzo_domain.name d)); Ok Brzo.Exit.ok

(* Command line interface *)

open Cmdliner

let doc = "Show selected default domain"
let sdocs = Manpage.s_common_options
let exits = Brzo.Exit.Info.undefined_domain :: Brzo.Exit.Info.base_cmd
let man_xrefs = [ `Main ]
let man = [
  `S Manpage.s_description;
  `P "The $(tname) command shows the default domain of a $(mname) \
      invocation. The default domain is either automatically selected \
      according to the sources that are present in the BRZO root or \
      explicitely set by the BRZO file.";
  `P "See the manual in $(b,odig doc brzo) for more details.";
  Brzo.Cli.man_see_manual; ]

let cmd =
  Cmd.v (Cmd.info "domain" ~doc ~sdocs ~exits ~man ~man_xrefs)
    Term.(const domain $ Brzo_tie_conf.use_brzo_file)

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
