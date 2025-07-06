(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
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
