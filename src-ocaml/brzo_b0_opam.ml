(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00

let tool = Tool.by_name ~vars:["PATH"] "opam"
let opam_bin = Cmd.arg "opam"

(* FIXME memo is used here but only for future proofing but we should
   really use it. Also we need an easy no caching option spawns in Memo. *)

let exists m k = match Os.Cmd.find opam_bin |> Memo.fail_if_error m with
| None -> k false | Some _ -> k true

let if_exists m f k =
  exists m @@ function false -> k None | true -> f () (fun v -> k (Some v))

let run m ?switch:s cmd oargs k =
  let opam = Os.Cmd.must_find opam_bin |> Memo.fail_if_error m in
  let s = match s with None -> Cmd.empty | Some s -> Cmd.(arg "--switch" % s) in
  k (Os.Cmd.run_out ~trim:true Cmd.(opam % cmd %% s %% oargs))

let lib_dir m ?switch () k =
  run m ?switch "var" Cmd.(arg "lib") @@ fun r ->
  k @@ Memo.fail_if_error m @@ Result.bind r @@ fun s -> Fpath.of_string s

let list m ?switch which () k =
  let which = match which with
  | `Available -> Cmd.arg "--available"
  | `Installed -> Cmd.arg "--installed"
  in
  run m ?switch "list" Cmd.(which % "--short") @@ fun r ->
  k @@ Memo.fail_if_error m @@
  Result.bind r @@ fun s -> Ok (B00_lines.of_string (String.trim s))


(* FIXME *)
type pkg = string * string option
let pkg_list ?switch:s () =
  let parse_pkg n s acc = match String.cut_left ~sep:" " s with
  | None -> B00_lines.err n " Cannot parse package from %S" s
  | Some (pkg, version) ->
      let pkg = match String.trim version with
      | "--" -> (String.trim pkg, None)
      | v -> (String.trim pkg, Some v)
      in
      pkg :: acc
  in
  Result.bind (Os.Cmd.must_find opam_bin) @@ fun opam ->
  let s = match s with None -> Cmd.empty | Some s -> Cmd.(arg "--switch" % s) in
  let list =
    Cmd.(opam % "list" %% s % "--columns=name,installed-version"  %
         "--short" % "--normalise" % "--all")
  in
  Result.bind (Os.Cmd.run_out ~trim:true list) @@ fun pkg_list ->
  B00_lines.fold pkg_list parse_pkg []

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
