(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

let delete c clean =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let b0_dir = Brzo.Conf.b0_dir c in
  let del_dir = match clean with
  | true -> b0_dir
  | false -> Fpath.(b0_dir / Brzo.Conf.brzo_dir_name)
  in
  Result.bind (Os.Path.delete ~recurse:true del_dir) @@ fun _ ->
  Ok Brzo.Exit.ok

(* Command line interface *)

open Cmdliner

let doc = "Delete builds"
let sdocs = Manpage.s_common_options
let exits = Brzo.Exit.Info.base_cmd
let man_xrefs = [ `Main; `Cmd "cache" ]
let man = [
  `S Manpage.s_description;
  `P "The $(tname) command deletes all outcome builds and artefacts. The \
      build cache is however kept intact.";
  `P "Use option $(b,--clean) option to get rid of the $(b,_b0) directory \
      altogeter. The will likely also get rid of the build cache. \
      See $(mname) $(b,cache) for finer control over build cache deletions.";
  Brzo.Cli.man_see_manual; ]

let clean =
  let doc = "Delete the $(b,_b0) directory." in
  Arg.(value & flag & info ["c"; "clean"] ~doc)

let cmd =
  Term.(const delete $ Brzo_tie_conf.auto_cwd_root_and_no_brzo_file $ clean),
  Term.info "delete" ~doc ~sdocs ~exits ~man ~man_xrefs

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
