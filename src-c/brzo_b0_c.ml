(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Fut.Syntax
open B00

module Tool = struct
  let gcc_env_vars =
    (* See https://gcc.gnu.org/onlinedocs/gcc/Environment-Variables.html *)
    [ "GCC_EXEC_PREFIX"; "COMPILER_PATH"; "LANG"; "LIBRARY_PATH"; "CPATH";
      "C_INCLUDE_PATH"; "CPLUS_INCLUDE_PATH"; "OBJC_INCLUDE_PATH";
      "SOURCE_DATE_EPOCH" ]

  let gcc = Tool.by_name ~vars:gcc_env_vars "gcc"
end

module Conf = struct
  let obj_ext m = Fut.return ".o" (* FIXME *)
end

module Inc_deps = struct
  let rec skip_white max s i = (* skips white and bslash (lf | cr) *)
    if i > max then i else
    if Char.Ascii.is_white s.[i] then skip_white max s (i + 1) else
    if s.[i] = '\\' && i + 1 <= max && (s.[i+1] = '\n' || s.[i+1] = '\r')
    then skip_white max s (i + 2) else i

  let rec find_start max s i =
    if i > max then i else
    if s.[i] = ':' then i + 1 else find_start max s (i + 1)

  let parse_path root max s start =
    let path s first last =
      let token = String.subrange ~first ~last s in
      Fpath.(root // (of_string token |> Result.to_failure))
    in
    let rec loop max s i =
      if i > max then i, path s start (i - 1) else
      match Char.Ascii.is_white s.[i] with
      | false -> loop max s (i + 1)
      | true when s.[i] = ' ' && s.[i-1] = '\\' -> loop max s (i + 1)
      | true -> i, path s start (i - 1)
    in
    loop max s start

  let of_string ?(file = Fpath.null) ~root s =
    let rec loop root acc max s i =
      let i = skip_white max s i in
      if i > max then List.rev acc else
      let i, p = parse_path root max s i in
      loop root (p :: acc) max s i
    in
    let max = String.length s - 1 in
    try Ok (loop root [] max s (find_start max s 0)) with
    | Failure e -> Fmt.error "%a: %s" Fpath.pp_unquoted file e

  let write ?(deps = []) m ~src ~o =
    let gcc = Memo.tool m Tool.gcc in
    let reads = src :: deps and writes = [o] in
    Memo.spawn m ~reads ~writes @@
    gcc Cmd.(arg "-M" % "-MF" %% path o %% path src)

  let read m ~src file =
    let* s = Memo.read m file in
    let deps = of_string ~file ~root:(Fpath.parent src) s in
    let deps = Memo.fail_if_error m deps in
    Fut.return deps
end

module Compile = struct
  let c_to_o ?post_exec ?k ?args:(more = Cmd.empty) m ~deps ~c ~o =
    let gcc = Memo.tool m Tool.gcc in
    Memo.spawn m ?post_exec ?k ~reads:(c :: deps) ~writes:[o] @@
    gcc Cmd.(more % "-c" % "-o" %% unstamp (path o %% path c))
end

module Link = struct
  let exe ?post_exec ?k ?args:(more = Cmd.empty) m ~objs ~o =
    let gcc = Memo.tool m Tool.gcc in
    Memo.spawn m ?post_exec ?k ~reads:objs ~writes:[o] @@
    gcc Cmd.(more % "-o" %% unstamp (path o %% paths objs))
end

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers

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
