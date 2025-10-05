(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_sexp
open Fut.Syntax
open Cmdliner

(* Targets *)

type target = [ `Byte | `Html | `Native | `Node ]
let target_to_string = function
| `Byte -> "byte" | `Native -> "native" | `Html -> "html" | `Node -> "node"

let default_target ~default_native_if:tool m target = match target with
| Some c -> Fut.return c
| None ->
    let* tool = B0_memo.tool_opt m tool in
    Fut.return (if Option.is_some tool then `Native else `Byte)

(* Domain configuration *)

type t =
  { target : target option;
    ocamlpath : Fpath.t list;
    libs : Fpath.t list;
    lock_libs : bool; }

let target c = c.target
let ocamlpath c = c.ocamlpath
let libs c = c.libs
let lock_libs c = c.lock_libs
let pp =
  let pp_target ppf t = Fmt.string ppf (target_to_string t) in
  Fmt.record
    [ Fmt.field "target" target (Brzo.Conf.pp_auto pp_target);
      Fmt.field "ocamlpath" ocamlpath (Fmt.vbox (Fmt.list Fpath.pp_quoted));
      Fmt.field "libs" libs (Fmt.vbox (Fmt.list Fpath.pp_quoted));
      Fmt.field "lock-libs" lock_libs Fmt.bool; ]

let tid = Type.Id.make ()
let docs = Brzo.Cli.s_outcome_opts

let target_key = "target"
let target_c =
  let targets =
    [ "byte", `Byte,
      "Bytecode outcome artefact. For the $(b,exec) and $(b,top) outcomes.";
      "native", `Native,
      "Native code outcome artefact. For the $(b,exec) and $(b,top) outcomes.";
      "html", `Html,
      "HTML file outcome artefact and execution via a browser. For the \
       $(b,exec) and $(b,top) outcomes.";
      "node", `Node,
      "JavaScript outcome artefact and execution via node. For the $(b,exec) \
       outcome."]
  in
  let cli =
    let target_arg (arg, v, doc) = Some (Some v), Arg.info [arg] ~docs ~doc in
    let add_target acc t = target_arg t :: acc in
    Arg.(value & vflag None (List.fold_left add_target [] targets))
  in
  let conf =
    let add_target acc (k, v, _) = String.Map.add k v acc in
    let m = List.fold_left add_target String.Map.empty targets in
    Sexpq.(some (atomic @@ enum_map ~kind:target_key m))
  in
  Brzo.Conf.Bit.with_cli target_key ~absent:None ~conf ~cli

let ocamlpath_key = "ocamlpath"
let ocamlpath_c =
  let conf = Sexpq.(list Brzo.Sexp.fpath) in
  let cli = Term.const None
    (*
    let doc =
      "Lookup in $(docv) for external dependencies. Repeatable and ordered. If \
       unspecified look in $(b,ocamlc -where) and $(b,opam var lib)."
    in
    Term.(const Option.some $
          Arg.(value & opt_all B0_std_ui.fpath [] &
               info ["lib-dir"] ~doc ~docs ~docv:"DIR"))
    *)
  in
  Brzo.Conf.Bit.with_cli ocamlpath_key ~absent:[] ~conf ~cli

let libs_key = "libs"
let libs_c =
  let parse_lib s =
    Result.bind (Fpath.of_string s) @@ fun p -> match Fpath.is_relative p with
    | true -> Ok (Fpath.drop_trailing_dir_sep p)
    | false -> Fmt.error "%a: not a library name" Fpath.pp_unquoted p
  in
  let conf = Sexpq.(list (atom_to ~kind:"lib" parse_lib)) in
  let cli =
    let lib =
      let parse s = Result.map_error (fun e -> `Msg e) (parse_lib s) in
      let pp ppf p = Fmt.(code' Fpath.pp_unquoted) ppf p in
      Arg.conv ~docv:"LIB" (parse, pp)
    in
    let doc =
      "Add external library restriction $(docv). Repeatable and ordered."
    in
    let docv = "LIB" in
    let libs = Arg.(value & opt_all lib [] & info ["lib"] ~doc ~docs ~docv) in
    Term.(const Option.some $ libs)
  in
  Brzo.Conf.Bit.with_cli libs_key ~absent:[] ~conf ~cli

let lock_libs_key = "lock-libs"
let lock_libs_c =
  Brzo.Conf.Bit.with_cli_arg lock_libs_key
    ~doc:"Only lookup dependencies in library restrictions."
    ~docv:"BOOL" ~docs
    ~absent:false
    ~conf:Sexpq.(atomic bool)
    ~arg:Arg.(opt ~vopt:(Some true) (some bool) (Some false))

let get_conf target ocamlpath libs lock_libs sexp =
  Result.bind (Brzo.Conf.Bit.get target_c target sexp) @@ fun target ->
  Result.bind (Brzo.Conf.Bit.append ocamlpath_c ocamlpath sexp) @@ fun p ->
  Result.bind (Brzo.Conf.Bit.append libs_c libs sexp) @@ fun libs ->
  Result.bind (Brzo.Conf.Bit.get lock_libs_c lock_libs sexp) @@ fun lock_libs ->
  Ok { target; ocamlpath = p; libs; lock_libs }

let keys =
  String.Set.of_list [target_key; ocamlpath_key; libs_key; lock_libs_key]

let parse = get_conf None None None None
let parse_with_cli =
  Term.(const get_conf $ Brzo.Conf.Bit.cli target_c $
        Brzo.Conf.Bit.cli ocamlpath_c $ Brzo.Conf.Bit.cli libs_c $
        Brzo.Conf.Bit.cli lock_libs_c)
