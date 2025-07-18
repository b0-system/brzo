(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let domain ~conf =
  Log.if_error ~use:Brzo.Exit.undefined_domain @@
  let* d = Brzo_domain.of_conf conf Brzo_domain_list.v in
  Log.stdout (fun m -> m "@[%s@]" (Brzo_domain.name d));
  Ok Brzo.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Show selected default domain" in
  let exits = Brzo.Exit.Info.undefined_domain :: Brzo.Exit.Info.base_cmd in
  let man = [
    `S Manpage.s_description;
    `P "The $(cmd) command shows the default domain of a $(tool) \
        invocation. The default domain is either automatically selected \
        according to the sources that are present in the BRZO root or \
        explicitely set by the BRZO file.";
    `P "See the manual in $(b,odig doc brzo) for more details.";
    Brzo.Cli.man_see_manual; ]
  in
  Cmd.make (Cmd.info "domain" ~doc ~exits ~man) @@
  let+ conf = Brzo_tie_conf.use_brzo_file in
  domain ~conf
