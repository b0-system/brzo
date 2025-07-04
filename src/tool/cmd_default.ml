(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

(* We make a fake domain to benefit of some of system's domain documentation
   support. *)

module Default = struct
  module Conf = struct
    type t = unit
    let tid = Type.Id.make ()
    let get _ = Ok ()
    let keys = String.Set.empty
    let parse = get
    let parse_with_cli = Cmdliner.Term.const get
    let pp = Fmt.nop
  end
  let name = "default"
  let doc_name = "default"
  let fingerprint = String.Set.empty
  let outcomes = (* Fake outcomes for docs never actually used *)
    let nop name arg doc  =
      let artefact m _ _ ~build_dir = Fut.return build_dir in
      let build m _ _ ~build_dir ~artefact ~srcs = Fut.return () in
      let action m _ _ ~build_dir ~artefact =
        Fut.return (fun () -> assert false)
      in
      Brzo_outcome.v ~name ~doc ~artefact ~build ~action_has_args:arg ~action ()
    in
    [ nop "exec" true "$(b,exec) outcome of the default domain (default).";
      nop "doc" false "$(b,doc) outcome of the default domain." ]

  let pre_outcomes = List.map Brzo_outcome.pre_outcome outcomes
end

let default ~conf =
  Log.if_error ~use:Brzo.Exit.undefined_domain @@
  match Brzo_domain.of_conf conf Brzo_domain_list.v with
  | Error _ when Brzo.Conf.outcome_mode conf = `Conf ->
      Log.stdout (fun m -> m "%a" Brzo.Conf.pp_show conf); Ok Brzo.Exit.ok
  | r -> Result.bind r @@ fun domain -> Ok (Brzo_domain.run conf domain)

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let term =
  let domain = Brzo_domain.V (module Default) in
  let+ conf = Brzo_tie_conf.for_domain ~domain in
  default ~conf

let cmd =
  let doc = "Default domain (default command)" in
  let docs = Brzo.Cli.s_domains in
  let exits = Brzo.Exit.Info.domain_cmd in
  let man = [
    `S Manpage.s_description;
    `P "The $(cmd) command has the outcomes for the default domain.";
    `P "The default domain is either automatically selected according to \
        the sources that are present in the BRZO root or explicitely set \
        by the BRZO file. Use $(b,brzo domain) to determine which default
      domain is selected.";
    `P "See the manual in $(b,odig doc brzo) for more details.";
    `S Manpage.s_arguments;
    `S Brzo.Cli.s_outcomes;
    `S Brzo.Cli.s_outcome_mode;
    Brzo.Cli.man_see_manual; ]
  in
  Cmd.make (Cmd.info "default" ~doc ~docs ~exits ~man) term
