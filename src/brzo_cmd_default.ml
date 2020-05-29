(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

(* We make a fake domain to benefit of some of system's domain documentation
   support. *)

module Default = struct
  module Conf = struct
    type t = unit
    let tid = Tid.create ()
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

let default c =
  Log.if_error ~use:Brzo.Exit.undefined_domain @@
  match Brzo_domain.of_conf c Brzo_domain_list.v with
  | Error _ when Brzo.Conf.outcome_mode c = `Conf ->
      Log.app (fun m -> m "%a" Brzo.Conf.pp_show c); Ok Brzo.Exit.ok
  | r -> Result.bind r @@ fun domain -> Ok (Brzo_domain.run c domain)


(* Command line interface *)

open Cmdliner

let doc = "Default domain (default command)" and docs = Brzo.Cli.s_domains
let sdocs = Manpage.s_common_options
let exits = Brzo.Exit.Info.domain_cmd
let man_xrefs = [ `Main ]
let man = [
  `S Manpage.s_description;
  `P "The $(tname) command has the outcomes for the default domain.";
  `P "The default domain is either automatically selected according to \
      the sources that are present in the BRZO root or explicitely set \
      by the BRZO file. Use $(b,brzo domain) to determine which default
      domain is selected.";
  `P "See the manual in $(b,odig doc brzo) for more details.";
  `S Manpage.s_arguments;
  `S Brzo.Cli.s_outcomes;
  `S Brzo.Cli.s_outcome_mode;
  Brzo.Cli.man_see_manual; ]

let cmd =
  let domain = Brzo_domain.V (module Default) in
  Term.(const default $ Brzo_tie_conf.for_domain ~domain),
  Term.info "default" ~doc ~docs ~sdocs ~exits ~man ~man_xrefs

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
