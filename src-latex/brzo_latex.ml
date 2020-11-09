(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Fut.Syntax
open B00
open B00_serialk_sexp
open Brzo_b0_latex

module Conf = struct
  type t =
    { curl : Cmd.t;
      doi_resolver : B00_http.Uri.t;
      main : Fpath.t option; }

  let curl c = c.curl
  let doi_resolver c = c.doi_resolver
  let main c = c.main
  let pp =
    Fmt.record
      [ Fmt.field "curl" curl Cmd.pp;
        Fmt.field "main" main (Brzo.Conf.pp_auto Fpath.pp_quoted);
        Fmt.field "doi-resolver" doi_resolver Fmt.string; ]

  let tid : t Tid.t = Tid.create ()
  let docs = Brzo.Cli.s_outcome_opts

  let curl_key = "curl"
  let curl_c =
    Brzo.Conf.Bit.with_cli_arg curl_key
      ~doc:"Use $(docv) to curl" ~docv:"CMD" ~docs
      ~absent:(Cmd.atom "curl")
      ~conf:Sexpq.(map Cmd.list (list atom))
      ~arg:Cmdliner.Arg.(opt (some ~none:"curl" B00_cli.cmd) None)

  let doi_resolver_key = "doi-resolver"
  let doi_resolver_c =
    Brzo.Conf.Bit.with_cli_arg doi_resolver_key
      ~doc:"Use $(docv) to resolve DOIs." ~docv:"URI" ~docs
      ~absent:Doi.default_resolver
      ~conf:Sexpq.(atomic atom)
      ~arg:(Cmdliner.Arg.(opt (some ~none:Doi.default_resolver string) None))

  let main_key = "main"
  let main_c =
    Brzo.Conf.Bit.with_cli_arg main_key
      ~doc:"Use $(docv) as the main $(b,.tex) file. If unspecified this \
            is guessed see $(b,odig doc brzo) for details."
      ~docv:"FILE" ~docs
      ~absent:None
      ~conf:Sexpq.(some (atomic Brzo.Sexp.fpath))
      ~arg:Cmdliner.Arg.(opt (some ~none:"guessed"
                                (some B00_cli.fpath)) None)

  let get_conf curl doi_resolver main sexp =
    Result.bind (Brzo.Conf.Bit.get curl_c curl sexp) @@ fun curl ->
    Result.bind (Brzo.Conf.Bit.get doi_resolver_c doi_resolver sexp) @@
    fun doi_resolver ->
    Result.bind (Brzo.Conf.Bit.get main_c main sexp) @@ fun main ->
    Ok { curl; doi_resolver; main }

  let keys = String.Set.of_list [curl_key; doi_resolver_key; main_key]
  let parse = get_conf None None None
  let parse_with_cli =
    Cmdliner.Term.(pure get_conf $ Brzo.Conf.Bit.cli curl_c $
                   Brzo.Conf.Bit.cli doi_resolver_c $ Brzo.Conf.Bit.cli main_c)
end

let name = "latex"
let doc_name = "LaTeX"
let fingerprint = B00_fexts.latex_lang

(* Outcomes *)

let bibdoi_to_bib m c dc bibdoi =
  let bib = Fpath.(bibdoi -+ ".bib") in
  let httpr = B00_http.Httpr.get_curl ~curl:dc.Conf.curl () in
  begin
    Memo.file_ready m bibdoi;
    ignore @@
    let* contents = Memo.read m bibdoi in
    begin
      Memo.write m ~stamp:"%%VERSION%%" ~reads:[bibdoi] bib @@ fun () ->
      Result.bind (Bibdoi.of_string ~file:bibdoi contents) @@ fun b ->
      Result.bind httpr @@ fun r ->
      (* We should memoize each curl invocation *)
      Bibdoi.to_bibtex ~resolver:(Conf.doi_resolver dc) r b
    end;
    Fut.return ()
  end;
  bib

let find_main m c dc ~texs =
  let not_found m =
    Memo.fail m
      "@[<v>Could not detect a main tex file.@, Use %a to specify one.@]"
      Fmt.(code string) "--main"
  in
  let find_basename name texs =
    let has_basename n p = String.equal n (Fpath.basename p) in
    match List.filter (has_basename name) texs with
    | [] -> None
    | [t] -> Some t
    | t :: _ ->
        Memo.notify m `Warn "@[<v>Found multiple %a sources using:@,%a@]"
          Fmt.(code string) name Fpath.pp_quoted t;
        Some t
  in
  match Conf.main dc with
  | Some main ->
      let main = Fpath.(Brzo.Conf.root c // main) in
      if List.mem main texs then Fut.return main else
      Memo.fail m "Main file %a not found in %a sources."
        Fmt.(code Fpath.pp_unquoted) main Fmt.(code string) ".tex"
  | None ->
      match texs with
      | [] -> failwith "TODO if there's bib create a doc with the whole bib"
      | [t] -> Fut.return t
      | texs ->
          match find_basename "main.tex" texs with
          | Some t -> Fut.return t
          | None ->
              let root_name = Fpath.basename ~no_ext:true (Brzo.Conf.root c) in
              match find_basename (Fmt.str "%s.tex" root_name) texs with
              | Some t -> Fut.return t
              | None -> not_found m

(*

    let xelatex = Memo.tool m Tool.xelatex in
    let pdf = Fpath.(dir / Fmt.str "%s.pdf" oname) in
    let fls = Fpath.(dir / Fmt.str "%s.fls" oname) in
    Memo.spawn m ~reads:[tex] ~writes:[pdf; fls] @@
    xelatex Cmd.(arg "-file-line-error" % "-halt-on-error" %
                 "-interaction=errorstopmode" %
                 "-output-directory" %% path dir %% more_args %
                 Fmt.str "-jobname=%s" oname %% path tex)
*)


let exec_build m c dc ~build_dir ~artefact ~srcs =
  let stys = B00_fexts.(find_files (ext ".sty") srcs) in
  let texs = B00_fexts.(find_files tex srcs) in
  let bibs = B00_fexts.(find_files (ext ".bib") srcs) in
  let bibdois = B00_fexts.(find_files (ext ".bibdoi") srcs) in
  let doibibs = List.rev_map (bibdoi_to_bib m c dc) bibdois in
  let* main = find_main m c dc ~texs in
  let xelatex = Memo.tool m Tool.xelatex in
  let _aux = Fpath.(artefact -+ ".aux") in
  let _toc = Fpath.(artefact -+ ".toc") in
  let _out = Fpath.(artefact -+ ".out") in
  List.iter (Memo.file_ready m) stys;
  List.iter (Memo.file_ready m) texs;
  List.iter (Memo.file_ready m) bibs;
  let cli =
    Cmd.(atom "-file-line-error" % "-halt-on-error" %
         "-interaction=nonstopmode" %
         "-output-directory" %% path build_dir %
         "-jobname" % Fpath.basename ~no_ext:true artefact %%
         path main)
  in
  let reads =
    List.(rev_append (rev_append bibs doibibs) (rev_append texs stys))
  in
  (* FIXME fixpoint *)
  let k _ = Os.Cmd.run Cmd.(atom "xelatex" %% cli) |> Result.get_ok in
  Memo.spawn m ~k ~reads ~writes:[] @@
  xelatex cli;
  Fut.return ()

let exec =
  let name = "exec" and doc = "Build and show a PDF (default)." in
  let artefact m _ _ ~build_dir =
    Fut.return (Fpath.(build_dir / "a.pdf"))
  in
  let build = exec_build in
  let action = Brzo_outcome.Action.show_pdf in
  Brzo_outcome.v ~name ~doc ~artefact ~build ~action_has_args:false ~action ()

let doc =
  let name = "doc" and doc = "Synonym for $(b,--exec)." in
  Brzo_outcome.with_outcome ~name ~doc exec

(* Listings outcome *)

let latex_srcs_doc = format_of_string
{latex|
\documentclass[oneside,a4paper,article]{memoir}

%%\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{textcomp}
\usepackage[scaled]{berasans}
\usepackage[scaled]{beramono}
\usepackage[hidelinks]{hyperref}
\usepackage{datetime2}

\addtolength{\topmargin}{-2cm}
\addtolength{\textheight}{3cm}
\addtolength{\textwidth}{2cm}
\addtolength{\oddsidemargin}{-1cm}

\renewcommand{\rmdefault}{fvs}

%% Title
\pretitle{\Huge\bfseries}
\posttitle{\par}
\predate{\normalsize\slshape}
\postdate{\par}

%% Chapter headings
\renewcommand{\chaptitlefont}{\huge\ttfamily\bfseries}
\renewcommand{\chapnumfont}{\huge\rmfamily\bfseries}
\setlength{\midchapskip}{0.2\onelineskip}

%% Toc
\renewcommand{\tocheadstart}{}
\renewcommand{\aftertoctitle}{}
\renewcommand{\printtoctitle}[1]{}
\setpnumwidth{0pt}
\setrmarg{0pt}
\renewcommand{\cftchapterfont}{\normalsize\ttfamily}
\setlength{\cftbeforechapterskip}{2\smallskipamount}
\renewcommand{\cftchapterpresnum}{\hfill\small\rmfamily\MakeLowercase}
\settowidth{\cftchapternumwidth}{A\quad}
\renewcommand{\cftchapteraftersnum}{\hspace*{10pt}}
\renewcommand{\cftchapteraftersnumb}{\MakeLowercase}
\newlength{\spacelength}
\settowidth{\spacelength}{\small\space }
\renewcommand{\cftchapterleader}{\hspace{-\spacelength}}
\renewcommand{\cftchapterpagefont}{\quad\rmfamily\slshape\scriptsize}

%% Code
\usepackage{listings}
\lstset{
  columns=[c]fixed,
  basicstyle=\small\ttfamily,
  keywordstyle=\bfseries,
  upquote=true,
  commentstyle=\slshape,
  breaklines=true,
  showstringspaces=false}

\lstdefinestyle{ocaml}{language=[objective]caml,
  literate={'"'}{\textquotesingle "\textquotesingle}3
            {'\\"'}{\textquotesingle \textbackslash"\textquotesingle}4,
}

%% Stolen and adapted from
%% https://github.com/ghammock/LaTeX_Listings_JavaScript_ES6
%% MIT License Copyright (c) 2018 Gary Hammock
\lstdefinelanguage{JavaScript}{
  morekeywords=[1]{break, continue, delete, else, for, function, if, in,
    new, return, this, typeof, var, void, while, with,
    await, async, case, catch, class, const, default, do,
    enum, export, extends, finally, from, implements, import, instanceof,
    let, static, super, switch, throw, try},
  morekeywords=[2]{false, null, true, boolean, number, undefined,
    Array, Boolean, Date, Math, Number, String, Object},
  morekeywords=[3]{eval, parseInt, parseFloat, escape, unescape},
  sensitive,
  morecomment=[s]{/*}{*/},
  morecomment=[l]//,
  morecomment=[s]{/**}{*/},
  morestring=[b]',
  morestring=[b]",
  morestring=[b]`
}[keywords, comments, strings]

\lstdefinelanguage{CSS}
{
morekeywords={inherit, initial, unset, @charset, @import, @namespace, @media,
              @font-face, @supports, @document, @page, @keyframes, @viewport},
morecomment=[l]{//},
morecomment=[s]{/*}{*/},
morestring=[b]',
morestring=[b]",
alsoletter={:},
alsodigit={-}
}

\title{{\ttfamily %s} listing}
\date{\today}

\begin{document}
\maketitle
\pdfbookmark{File list}{filelist}
\tableofcontents*
\newpage
%a
\end{document}
|latex}

let latex_src_chapter = format_of_string
{latex|
\chapter{%s}
\lstinputlisting[%s]{%a}
\newpage
|latex}

let langs =
  [ B00_fexts.c_lang, "language=C";
    B00_fexts.css, "language=CSS";
    B00_fexts.js, "language=JavaScript";
    B00_fexts.(ocaml_lang - ext ".mld"), "style=ocaml"; ]

let supported_srcs =
  let add_exts acc (exts, _) = String.Set.union acc exts in
  List.fold_left add_exts String.Set.empty langs

let latex_lang_setup src =
  let ext = Fpath.get_ext src in
  let is_lang (exts, _) = String.Set.mem ext exts in
  match List.find is_lang langs with
  | exception Not_found -> ""
  | (_, setup) -> setup

let src_listing ~src_root ppf src =
  let file = Option.get (Fpath.rem_prefix src_root src) in
  Fmt.pf ppf
    latex_src_chapter
    (Latex.escape (Fpath.to_string file))
    (latex_lang_setup file)
    Fpath.pp_unquoted src

let listing_doc ~src_root srcs =
  Fmt.str latex_srcs_doc Fpath.(basename src_root)
    Fmt.(list (src_listing ~src_root)) srcs

let write_listing m ~src_root srcs ~o =
  let doc = listing_doc ~src_root srcs in
  let stamp = String.concat ""
      [ string_of_format latex_srcs_doc;
        string_of_format latex_src_chapter]
  in
  Memo.write m ~stamp ~reads:srcs o @@ fun () -> Ok doc

let listings_artefact m c _ ~build_dir =
  let src_root = Brzo.Conf.root c in
  let name = Fpath.(basename src_root) in
  Fut.return Fpath.(build_dir / Fmt.str "%s-listing.pdf" name)

let sort_src_paths p0 p1 =
  let p0, ext0 = Fpath.cut_ext p0 and p1, ext1 = Fpath.cut_ext p1 in
  let c = Fpath.compare p0 p1 in
  if c <> 0 then c else
  match ext0, ext1 with
  | ".h", ".c" -> -1 | ".c", ".h" -> 1
  | ".mli", ".ml" -> -1 | ".ml", ".mli" -> 1
  | ext0, ext1 -> String.compare ext0 ext1

let listing_build m c _ ~build_dir ~artefact ~srcs =
  let srcs = B00_fexts.(find_files supported_srcs srcs) in
  let srcs = List.sort sort_src_paths srcs in
  let src_root = Brzo.Conf.root c in
  let xelatex = Memo.tool m Tool.xelatex in
  let doc_file = Fpath.(artefact -+ ".tex") in
  let aux = Fpath.(artefact -+ ".aux") in
  let toc = Fpath.(artefact -+ ".toc") in
  let out = Fpath.(artefact -+ ".out") in
  List.iter (Memo.file_ready m) srcs;
  write_listing m ~src_root srcs ~o:doc_file;
  let cli = Cmd.(atom "-file-line-error" % "-halt-on-error" %
                 "-interaction=nonstopmode" %
                 "-output-directory" %% path build_dir %% path doc_file)
  in
  (* FIXME fixpoint *)
  let k _ = Os.Cmd.run Cmd.(atom "xelatex" %% cli) |> Result.get_ok in
  Memo.spawn m ~reads:(doc_file :: srcs) ~writes:[artefact; aux; toc; out] ~k
  @@ xelatex cli;
  Fut.return ()

let listing =
  let name = "listing" in
  let exts =
    Fmt.(str "$(b,%a)" String.Set.(pp ~sep:comma string)) supported_srcs
  in
  let doc =
    Fmt.str "Build and show a PDF document with the contents of the source \
             files with extension %s found by $(mname) $(b,source)."
      exts
  in
  let artefact = listings_artefact in
  let build = listing_build in
  let action = Brzo_outcome.Action.show_pdf in
  Brzo_outcome.v ~name ~doc ~artefact ~build ~action_has_args:false ~action ()

(* Outcomes *)

let outcomes = [doc; exec; listing]
let pre_outcomes = List.map Brzo_outcome.pre_outcome outcomes

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
