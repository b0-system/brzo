(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

let exts_of_doms = function
| [] -> Ok None
| doms ->
    let add_domain acc d =
      let d = Result.to_failure (Brzo_domain.find d Brzo_domain_list.v) in
      String.Set.union (Brzo_domain.fingerprint d) acc
    in
    try Ok (Some (List.fold_left add_domain String.Set.empty doms)) with
    | Failure e -> Error e

let sources c doms =
  Log.if_error ~use:Brzo.Exit.undefined_domain @@
  Result.bind (exts_of_doms doms) @@ fun exts ->
  Log.if_error' ~use:Brzo.Exit.some_error @@
  Result.bind (B00_pager.find ~don't:(Brzo.Conf.no_pager c) ()) @@ fun pager ->
  Result.bind (B00_pager.page_stdout pager) @@ fun () ->
  Result.bind (Brzo.Conf.srcs c) @@ fun src_by_exts ->
  let add_files ext srcs acc = match exts with
  | None -> List.rev_append srcs acc
  | Some exts when String.Set.mem ext exts -> List.rev_append srcs acc
  | Some _ -> acc
  in
  let srcs = String.Map.fold add_files src_by_exts [] in
  match List.sort Fpath.compare srcs with
  | [] -> Ok Brzo.Exit.ok
  | srcs ->
      Log.app (fun m -> m "@[<v>%a@]" (Fmt.list Fpath.pp_unquoted) srcs);
      Ok Brzo.Exit.ok

(* Command line interface *)

open Cmdliner

let doc = "Show source files"
let sdocs = Manpage.s_common_options
let exits = Brzo.Exit.Info.undefined_domain :: Brzo.Exit.Info.base_cmd
let envs = B00_pager.envs ()
let man_xrefs = [ `Main ]
let man = [
  `S Manpage.s_description;
  `P "The $(tname) command lists the source files considered by a $(mname) \
      invocation.";
  Brzo.Cli.man_see_manual; ]

let doms =
  let doc =
    "Output files considered for selecting domain $(docv). Repeatable."
  in
  Arg.(value & opt_all string [] & info ["d"; "domain"] ~doc ~docv:"DOMAIN")

let cmd =
  Term.(const sources $ Brzo_tie_conf.use_brzo_file $ doms),
  Term.info "sources" ~doc ~sdocs ~exits ~envs ~man ~man_xrefs

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
