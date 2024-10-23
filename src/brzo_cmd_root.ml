(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std

let root c =
  Log.stdout (fun m -> m "%a" Fpath.pp_unquoted (Brzo.Conf.root c));
  Brzo.Exit.ok

(* Command line interface *)

open Cmdliner

let cmd =
  let doc = "Show root directory" in
  let exits = Brzo.Exit.Info.base_cmd in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) command shows the brzo root directory.";
    Brzo.Cli.man_see_manual; ]
  in
  Cmd.v (Cmd.info "root" ~doc ~exits ~man)
    Term.(const root $ Brzo_tie_conf.no_brzo_file)

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
