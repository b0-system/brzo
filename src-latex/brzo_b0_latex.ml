(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00

module Tool = struct
  let xelatex = Tool.by_name "xelatex"
  let bibtex = Tool.by_name "bibtex"
end

module Compile = struct
  let cmd m ?args:(more_args = Cmd.empty) ~tex ~dir ~oname =
    let xelatex = Memo.tool m Tool.xelatex in
    let pdf = Fpath.(dir / Fmt.str "%s.pdf" oname) in
    let fls = Fpath.(dir / Fmt.str "%s.fls" oname) in
    Memo.spawn m ~reads:[tex] ~writes:[pdf; fls] @@
    xelatex Cmd.(atom "-file-line-error" % "-halt-on-error" %
                 "-interaction=errorstopmode" %
                 "-output-directory" %% path dir %% more_args %
                 Fmt.str "-jobname=%s" oname %% path tex)
end

module Fls = struct
  type t = { reads : Fpath.Set.t; writes : Fpath.Set.t }
  let reads f = f.reads
  let writes f = f.writes
  let of_string ?file s =
    let parse_path i p =
      let rem_dot_seg s =
        (* Sometimes paths seem to show up with a leading ./ *)
        match String.length s > 2 && s.[0] = '.' && s.[1] = '/' with
        | true -> String.subrange ~first:2 s
        | false -> s
      in
      match Fpath.of_string (rem_dot_seg (String.trim p)) with
      | Ok p -> p
      | Error e -> B00_lines.err i "cannot parse path: %s" e
    in
    let parse_line i l = match String.cut_left ~sep:" " l with
    | None -> B00_lines.err i "cannot parse line: %S" l
    | Some ("INPUT", p) -> `Input (parse_path i p)
    | Some ("OUTPUT", p) -> `Output (parse_path i p)
    | Some ("PWD", p) ->
        let p = parse_path i p in
        if Fpath.is_abs p then `Cwd p else
        B00_lines.err i "PWD directive: %a not absolute" Fpath.pp_quoted p
    | Some (dir, _) -> B00_lines.err i "unknown directive: %S" dir
    in
    let rec loop i cwd reads writes = function
    | [] -> Ok { reads; writes }
    | l :: ls ->
        match parse_line i l with
        | `Cwd cwd ->
            loop (i + 1) cwd reads writes ls
        | `Input p ->
            loop (i + 1) cwd (Fpath.Set.add Fpath.(cwd // p) reads) writes ls
        | `Output p ->
            loop (i + 1) cwd reads (Fpath.Set.add Fpath.(cwd // p) writes) ls
    in
    try
      let lines = B00_lines.of_string s in
      loop 1 (Fpath.v "/") Fpath.Set.empty Fpath.Set.empty lines
    with
    | Failure e -> B00_lines.err_file ?file e
end

module Latex = struct
  let escape =
    let tilde_esc = "\\textasciitilde" in
    let tilde_len = String.length tilde_esc in
    let circ_esc = "\\textasciicircum" in
    let circ_len = String.length circ_esc in
    let bslash_esc = "\\textbackslash" in
    let bslash_len = String.length bslash_esc in
    let char_len = function
    | '&' | '%' | '$' | '#' | '_' | '{' | '}' -> 2
    | '~' -> tilde_len
    | '^' -> circ_len
    | '\\' -> bslash_len
    | _ -> 1
    in
    let set_char b i = function
    | '&' | '%' | '$' | '#' | '_' | '{' | '}' as c ->
        Bytes.set b i '\\'; Bytes.set b (i + 1) c; i + 2
    | '~' -> Bytes.blit_string tilde_esc 0 b i tilde_len; i + tilde_len
    | '^' -> Bytes.blit_string circ_esc 0 b i circ_len; i + circ_len
    | '\\' -> Bytes.blit_string bslash_esc 0 b i bslash_len; i + bslash_len
    | c -> Bytes.set b i c; i + 1
    in
    String.byte_escaper char_len set_char
end

module Doi = struct
  open B00_http

  let default_resolver = "https://doi.org"

  type t = string
  let pp = Fmt.string

  let resp_success req resp = match Http.resp_status resp with
  | 200 -> Ok (Http.resp_body resp)
  | st ->
      Fmt.error "%s on %s: responded with %d"
        (Http.meth_to_string (Http.req_meth req)) (Http.req_uri req) st

  let doi_uri ~resolver doi = Fmt.str "%s/%s" resolver doi

  let resolve_to_uri ?(resolver = default_resolver) r doi =
    let req = Http.req ~uri:(doi_uri ~resolver doi) `GET in
    Result.bind (Httpr.perform ~follow:false r req) @@ fun resp ->
    try Ok (List.assoc "location" (Http.resp_headers resp)) with
    | Not_found -> Error "No 'location' header found in response"

  let default_bib_format = "application/x-bibtex; charset=utf-8"
  let oneline_bib_format = "text/bibliography; charset=utf-8"
  let resolve_to_bib
      ?(format = default_bib_format) ?(resolver = default_resolver) r doi
    =
    let headers = ["Accept", format] in
    let req = Http.req ~headers ~uri:(doi_uri ~resolver doi) `GET in
    Result.bind (Httpr.perform r req) @@ fun resp ->
    resp_success req resp
end

module Bibdoi = struct
  open B00_serialk_sexp

  type t = { sexp : Sexp.t; dois : Doi.t list }
  let sexp b = b.sexp
  let dois b = b.dois

  let parse_doi a = match B00_http.Uri.parse_scheme a with
  | Some ("http" | "https") ->
      begin match B00_http.Uri.parse_path_and_query a with
      | None -> Fmt.error "%s: could not parse DOI" a
      | Some doi -> Ok doi
      end
  | Some scheme -> Fmt.error "%s: unknown URI scheme" scheme
  | None -> Ok a

  let qdoi = Sexpq.atom_to ~kind:"DOI" parse_doi
  let qdois = Sexpq.list qdoi

  let of_string ?(file = Fpath.dash) s =
    let open B00_serialk_sexp in
    let file = Fpath.to_string file in
    Result.bind (Sexp.seq_of_string' ~file s) @@ fun sexp ->
    Result.bind (Sexpq.query' qdois sexp) @@ fun dois ->
    Ok { sexp; dois }

  let to_bibtex ?resolver r b =
    let rec loop ?resolver r acc = function
    | [] -> String.concat "\n" (List.rev acc)
    | doi :: dois ->
        let bib = Doi.resolve_to_bib ?resolver r doi |> Result.to_failure in
        loop ?resolver r (bib :: acc) dois
    in
    try Ok (loop ?resolver r [] b.dois) with
    | Failure e -> Error e
end

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers

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
