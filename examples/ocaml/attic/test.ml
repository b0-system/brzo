(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std

let () =
  let files = List.map Fpath.v (List.tl (Array.to_list Sys.argv)) in
  let pp_dep ppf (b, n) = match b with
  | true -> Fmt.pf ppf "(open %s)" n
  | false -> Fmt.string ppf n
  in
  let pp_id = Fmt.(list ~sep:(unit ".") string) in
  let pp_deps = Fmt.(box @@ list ~sep:sp pp_dep) in
  let pp_ids = Fmt.(vbox @@ list pp_id) in
  let pp_file ppf f = match Fpath.get_ext f with
  | ".ml" ->
      pp_deps ppf (Brzo_ocaml.ml_deps f |> Result.to_failure)
  | ".mli" ->
      pp_deps ppf (Brzo_ocaml.mli_deps f |> Result.to_failure)
  | ".cmi" ->
      pp_ids ppf (Brzo_ocaml.cmi_defs f |> Result.to_failure)
  | _ -> ()
  in
  Fmt.pr "@[<v>%a@]" Fmt.(list pp_file) files


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
