(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

module Tool = struct
  let xelatex = B0_memo.Tool.by_name ~vars:["PATH"] "xelatex"
  let bibtex = B0_memo.Tool.by_name "bibtex"
end

module Compile = struct
  let cmd m ?args:(more_args = Cmd.empty) ~tex ~dir ~oname () =
    let xelatex = B0_memo.tool m Tool.xelatex in
    let pdf = Fpath.(dir / Fmt.str "%s.pdf" oname) in
    let fls = Fpath.(dir / Fmt.str "%s.fls" oname) in
    B0_memo.spawn m ~reads:[tex] ~writes:[pdf; fls] @@
    xelatex Cmd.(arg "-file-line-error" % "-halt-on-error" %
                 "-interaction=errorstopmode" %
                 "-output-directory" %% path dir %% more_args %
                 Fmt.str "-jobname=%s" oname %% path tex)
end

module Fls = struct
  type t = { reads : Fpath.Set.t; writes : Fpath.Set.t }
  let reads f = f.reads
  let writes f = f.writes
  let of_string ?(file = Fpath.dash) s =
    let parse_path i p =
      let rem_dot_seg s =
        (* Sometimes paths seem to show up with a leading ./ *)
        match String.length s > 2 && s.[0] = '.' && s.[1] = '/' with
        | true -> String.subrange ~first:2 s
        | false -> s
      in
      match Fpath.of_string (rem_dot_seg (String.trim p)) with
      | Ok p -> p
      | Error e -> Fmt.failwith_line i " Cannot parse path: %s" e
    in
    let parse_line i l = match String.split_first ~sep:" " l with
    | None -> Fmt.failwith_line i " Cannot parse line: %S" l
    | Some ("INPUT", p) -> `Input (parse_path i p)
    | Some ("OUTPUT", p) -> `Output (parse_path i p)
    | Some ("PWD", p) ->
        let p = parse_path i p in
        if Fpath.is_absolute p then `Cwd p else
        Fmt.failwith_line i " PWD directive: %a not absolute" Fpath.pp_quoted p
    | Some (dir, _) -> Fmt.failwith_line i " Unknown directive: %S" dir
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
      (* TODO if there's no muliline parse use fold directly *)
      let add_line _ acc l = l :: acc in
      let rlines = String.fold_ascii_lines ~strip_newlines:true add_line [] s in
      loop 1 (Fpath.v "/") Fpath.Set.empty Fpath.Set.empty (List.rev rlines)
    with
    | Failure e -> Fpath.error file "%s" e
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
  open B0_http

  let default_resolver = "https://doi.org"

  type t = string
  let pp = Fmt.string

  let response_success request response =
    match Http.Response.status response with
    | 200 -> Ok (Http.Response.body response)
    | status ->
        let method' = Http.method_to_string (Http.Request.method' request) in
        let url = Http.Request.url request in
        Fmt.error "%s on %s: responded with %d" method' url status

  let doi_url ~resolver doi = Fmt.str "%s/%s" resolver doi

  let resolve_to_url ?(resolver = default_resolver) httpc doi =
    let request = Http.Request.make ~url:(doi_url ~resolver doi) `GET in
    let* response = Http_client.request ~follow:false httpc request in
    try Ok (List.assoc "location" (Http.Response.headers response)) with
    | Not_found -> Error "No 'location' header found in response"

  let default_bib_format = "application/x-bibtex; charset=utf-8"
  let oneline_bib_format = "text/bibliography; charset=utf-8"
  let resolve_to_bib
      ?(format = default_bib_format) ?(resolver = default_resolver) httpc doi
    =
    let headers = ["Accept", format] in
    let url = doi_url ~resolver doi in
    let request = Http.Request.make ~headers ~url `GET in
    let* response = Http_client.request ~follow:true httpc request in
    response_success request response
end

module Bibdoi = struct
  open B0_sexp

  type t = { sexp : Sexp.t; dois : Doi.t list }
  let sexp b = b.sexp
  let dois b = b.dois

  let parse_doi a = match B0_url.scheme a with
  | Some ("http" | "https") ->
      begin match B0_url.path a with
      | None -> Fmt.error "%s: could not parse DOI" a
      | Some doi -> Ok doi
      end
  | Some scheme -> Fmt.error "%s: unknown URI scheme" scheme
  | None -> Ok a

  let qdoi = Sexpq.atom_to ~kind:"DOI" parse_doi
  let qdois = Sexpq.list qdoi

  let of_string ?(file = Fpath.dash) s =
    let open B0_sexp in
    let file = Fpath.to_string file in
    Result.bind (Sexp.seq_of_string' ~file s) @@ fun sexp ->
    Result.bind (Sexpq.query' qdois sexp) @@ fun dois ->
    Ok { sexp; dois }

  let to_bibtex ?resolver r b =
    let rec loop ?resolver r acc = function
    | [] -> String.concat "\n" (List.rev acc)
    | doi :: dois ->
        let bib =
          Doi.resolve_to_bib ?resolver r doi |> Result.error_to_failure
        in
        loop ?resolver r (bib :: acc) dois
    in
    try Ok (loop ?resolver r [] b.dois) with
    | Failure e -> Error e
end
