(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

let find_used_keys c =
  Result.bind (Os.Dir.exists (Brzo.Conf.cache_dir c)) @@ function
  | false -> Ok (String.Set.empty)
  | true ->
      Result.map_error (Fmt.str "Cannot determine used keys: %s") @@
      Result.bind (B00_cli.Memo.Log.read (Brzo.Conf.log_file c)) @@ fun l ->
      Ok (B00_cli.File_cache.keys_of_done_ops (B00_cli.Memo.Log.ops l))

let get_used_keys c = Result.value ~default:String.Set.empty (find_used_keys c)

let cache c (max_byte_size, pct) (action, keys) =
  let dir = Brzo.Conf.cache_dir c in
  let action = match action with
  | `Delete -> B00_cli.File_cache.delete dir keys
  | `Gc ->
      Result.bind (find_used_keys c) @@ fun used ->
      B00_cli.File_cache.gc ~dir ~used
  | `Keys -> B00_cli.File_cache.keys dir
  | `Path -> Log.app (fun m -> m "%a" Fpath.pp_unquoted dir); Ok true
  | `Stats -> B00_cli.File_cache.stats ~dir ~used:(get_used_keys c)
  | `Trim ->
      let used = get_used_keys c in
      B00_cli.File_cache.trim ~dir ~used ~max_byte_size ~pct
  in
  Log.if_error ~use:Brzo.Exit.some_error @@
  Result.bind action @@ fun _ -> Ok Brzo.Exit.ok

(* Command line interface *)

open Cmdliner

let doc = "Operate on the build cache"
let sdocs = Manpage.s_common_options
let exits = Brzo.Exit.Info.base_cmd
let man_xrefs = [ `Main; `Tool "b00-cache" ]
let man = [
  `S Manpage.s_synopsis;
  `P "$(mname) $(tname) $(i,ACTION) [$(i,OPTION)]...";
  `S Manpage.s_description;
  `P "The $(tname) command operates on the build cache.";
  `S "ACTIONS";
  `I ("$(b,delete) [$(i,KEY)]...", "Delete the cache or only the given keys.");
  `I ("$(b,gc)", "Only keep keys used by the last build.");
  `I ("$(b,keys)", "List cache keys.");
  `I ("$(b,path)", "Display the path to the cache (may not exist).");
  `I ("$(b,stats)", "Show cache statistics.");
  `I ("$(b,trim) [$(b,--to-pct) $(i,PCT)] [$(b,--to-mb) $(i,MB)]",
      "Trim the cache to the minimal budget specified. Without options \
       trims to 50% of the current size. Keys used by the last build \
       are preserved if possible.");
  Brzo.Cli.man_see_manual; ]

let action =
  let action =
    [ "delete", `Delete; "gc", `Gc; "keys", `Keys; "path", `Path;
      "stats", `Stats; "trim", `Trim ]
  in
  let doc =
    Fmt.str "The action to perform. $(docv) must be one of %s."
      (Arg.doc_alts_enum action)
  in
  let action = Arg.enum action in
  Arg.(required & pos 0 (some action) None & info [] ~doc ~docv:"ACTION")

let parse_cli =
  let keys = B00_cli.File_cache.keys_none_is_all ~pos_right:1 () in
  let parse_cli action keys =
    let has_keys = match keys with `Keys _ -> true | _ -> false in
    match has_keys && action <> `Delete with
    | false -> `Ok (action, keys)
    | true ->
        `Error (true,
                "too many argument, no positional arguments for this action")
  in
  Term.(ret (const parse_cli $ action $ keys))

let cmd =
  Term.(const cache $ Brzo_tie_conf.auto_cwd_root_and_no_brzo_file $
        B00_cli.File_cache.trim_cli () $ parse_cli),
  Term.info "cache" ~doc ~sdocs ~exits ~man ~man_xrefs

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
