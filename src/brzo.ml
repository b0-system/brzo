(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open B0_sexp

module Memo = struct

  open B0_std.Fut.Syntax

  let copy_file m ~src_root ~dst_root src =
    let dst = Fpath.reroot ~src_root ~dst_root src in
    B0_memo.ready_file m src;
    B0_memo.copy m src ~dst

  let ensure_exec_build m ~srcs ~need_ext =
    match B0_file_exts.find_files B0_file_exts.(ext need_ext) srcs = [] with
    | false -> Fut.return ()
    | true ->
        let bstr = Fmt.(code string) in
        B0_memo.fail m "@[<v>No %a file found, cannot build an executable.@,\
                         @[Check %a and %a.@]@]"
          bstr need_ext bstr "brzo sources" bstr "brzo --conf"

  let run ~with_log:log_file m fut =
    let write_build_log m = match log_file with
    | None -> ()
    | Some log_file ->
        Log.if_error ~use:() (B0_cli.Memo.Log.(write log_file (of_memo m)))
    in
    let hook () = write_build_log m in
    Os.Exit.on_sigint ~hook @@ fun () ->
    let exit, set = Fut.make () in
    B0_memo.run_proc m
      (fun () -> let* v = fut () in set v; Fut.return ());
    B0_memo.stir m ~block:true;
    let ret = match B0_memo.status m with
    | Ok () ->
        begin match Fut.value exit with
        | None -> (* should not happen *) Error ()
        | Some exit -> Ok exit
        end
    | Error e ->
        let read_howto = Fmt.any "brzo log -r " in
        let write_howto = Fmt.any "brzo log -w " in
        Log.err begin fun m ->
          m ~header:"" "@[%a@]"
            (B0_zero_conv.Op.pp_aggregate_error ~read_howto ~write_howto ()) e
        end;
        Error ()
    in
    Log.time (fun _ m -> m "deleting trash") begin fun () ->
      Log.if_error ~use:() (B0_memo.delete_trash ~block:false m)
    end;
    write_build_log m;
    ret

  let create ~hash_fun ~cwd ~cache_dir ~trash_dir ~jobs =
    let feedback =
      let op_howto ppf o = Fmt.pf ppf "brzo log --id %d" (B0_zero.Op.id o) in
      let show_op = Log.Info and show_ui = Log.Error and level = Log.level () in
      B0_cli.Memo.pp_leveled_feedback ~op_howto ~show_op ~show_ui ~level
        Fmt.stderr
    in
    B0_memo.make ~hash_fun ~cwd ~cache_dir ~trash_dir ~jobs ~feedback ()
end

module Exit = struct
  let conf_error = Os.Exit.Code 118
  let no_build_outcome = Os.Exit.Code 120
  let no_such_sexp_path = Os.Exit.Code 1
  let ok = Os.Exit.Code 0
  let outcome_action_error = Os.Exit.Code 122
  let outcome_build_error = Os.Exit.Code 121
  let some_error = Os.Exit.Code 123
  let undefined_domain = Os.Exit.Code 119

  module Info = struct
    let e c doc = Cmdliner.Cmd.Exit.info (Os.Exit.get_code c) ~doc
    let action_exec_exit =
      Cmdliner.Cmd.Exit.info 0 ~max:255
        ~doc:"on outcome action execution, the action exit code"

    let conf_error =
      e conf_error "on configuration error (e.g. missing BRZO file)"

    let no_build_outcome = e no_build_outcome "on missing build outcome"
    let no_such_sexp_path =
      e no_such_sexp_path "on non-existent s-expression path"

    let outcome_build_error = e outcome_build_error "on outcome build failure"
    let outcome_action_error =
      e outcome_action_error "on outcome action failure"

    let some_error = e some_error "on indiscriminate errors reported on stderr"
    let undefined_domain = e undefined_domain "on undefined domain"
    let base_cmd = conf_error :: some_error :: Cmdliner.Cmd.Exit.defaults
    let domain_cmd =
      action_exec_exit :: no_build_outcome ::
      outcome_build_error :: outcome_action_error :: undefined_domain ::
      base_cmd
  end
end

module Pre_domain = struct
  type outcome = { name : string; doc : string }
  let outcome ~name ~doc = { name; doc }
  let outcome_name o = o.name
  let outcome_doc o = o.doc

  module type CONF = sig
    type t
    val tid : t Type.Id.t
    val keys : String.Set.t
    val parse :
      B0_sexp.Sexp.t * B0_sexp.Sexpq.path ->
      (t, string) result

    val parse_with_cli :
      (B0_sexp.Sexp.t * B0_sexp.Sexpq.path ->
       (t, string) result) Cmdliner.Term.t
    val pp : t Fmt.t
  end

  module type T = sig
    module Conf : CONF
    val name : string
    val doc_name : string
    val fingerprint : B0_file_exts.t
    val pre_outcomes : outcome list
  end

  type t = V : (module T with type Conf.t = 'a) -> t
  let name (V (module D)) = D.name
  let outcome_names (V (module D)) =
    let add acc o = String.Set.add (outcome_name o) acc in
    List.fold_left add String.Set.empty D.pre_outcomes
end

module Sexp = struct
  let pp_loc = Fmt.code Sexp.pp_loc
  let pp_red = Fmt.(tty [`Bold; `Fg `Red])
  let pp_prefix ppf () = Fmt.pf ppf "%a: " (pp_red Fmt.string) "Error"
  let pp_read_error = Sexp.pp_error ~pp_loc ~pp_prefix ()
  let pp_query_error =
    let pp_em = Fmt.(code string) in
    let pp_key = pp_em in
    let pp_path = Sexpq.pp_path ~pp_loc ~pp_key () in
    let pp_error_kind = Sexpq.pp_error_kind ~pp_em ~pp_key () in
    Sexpq.pp_error ~pp_loc ~pp_path ~pp_error_kind ~pp_prefix ()

  let of_string ?file s =
    let file = Option.map Fpath.to_string file in
    Sexp.seq_of_string' ~pp_error:pp_read_error ?file s

  let of_file file =
    Result.bind (Os.File.read file) @@ fun s ->
    Result.bind (of_string ~file s) @@
    fun sexp -> Ok (sexp, [])

  let query q s =
    Sexpq.error_to_string ~pp_error:pp_query_error (Sexpq.query_at_path q s)

  let fpath = Sexpq.atom_to ~kind:"file path" Fpath.of_string
end

module Conf = struct

  (* Configuration bits *)

  module Bit = struct
    type 'a t = { conf : 'a Sexpq.t; cli : 'a option Cmdliner.Term.t }

    let confq name ~absent conf = Sexpq.key name ~absent conf
    let with_cli_arg ?docs ?docv name ~doc ~absent ~conf ~arg =
      let conf = confq name ~absent conf in
      let cli =
        let info = Cmdliner.Arg.info [name] ~doc ?docs ?docv in
        Cmdliner.Arg.value (arg info)
      in
      { conf; cli }

    let with_cli name ~absent ~conf ~cli =
      { conf = confq name ~absent conf; cli }

    let conf c = c.conf
    let cli c = c.cli
    let get c cli sexp = match cli with
    | Some arg -> Ok arg
    | None -> Sexp.query c.conf sexp

    let append c cli sexp =
      Result.bind (Sexp.query c.conf sexp) @@ function conf ->
      Ok (match cli with None -> conf | Some cli -> List.append conf cli)
  end

  let brzo_file_name = "BRZO"
  let brzo_dir_name = "brzo"

  (* Outcome modes *)

  type outcome_mode = [ `Action | `Build | `Conf | `Delete | `Normal | `Path ]
  let pp_outcome_mode ppf = function
  | `Action -> Fmt.string ppf "action"
  | `Build -> Fmt.string ppf "build"
  | `Conf -> Fmt.string ppf "conf"
  | `Delete -> Fmt.string ppf "delete"
  | `Normal -> Fmt.string ppf "normal"
  | `Path -> Fmt.string ppf "path"

  (* Domain configuration *)

  type domain =
  | Domain :
      ((module Pre_domain.T with type Conf.t = 'a)) * string  * 'a -> domain

  let collect_srcs ~srcs_i ~srcs_x = (* as a map from exts to file lists. *)
    Log.time (fun _ m -> m "source files collection") @@ fun () ->
    let auto_exclude = function
    | "" | "." | ".." -> false
    | "_b0" | "_build" -> true
    | s when s.[0] = '.' -> true
    | s -> false
    in
    let exclude fname file =
      if auto_exclude fname then not (Fpath.Set.mem file srcs_i) else
      Fpath.Set.mem file srcs_x
    in
    let add_file fname p (seen, by_ext as acc) =
      let ext = Fpath.get_ext p in
      if not (String.Set.mem ext B0_file_exts.all) then acc else
      seen, (String.Map.add_to_list ext p by_ext)
    in
    let add st fname p (seen, by_ext as acc) =
      if exclude fname p then acc else
      match st.Unix.st_kind with
      | Unix.S_DIR -> (Fpath.Set.add p seen, by_ext)
      | _ -> add_file fname p acc
    in
    let add_i (seen, by_ext as acc) i =
      match Os.File.exists i |> Result.error_to_failure with
      | true -> add_file (Fpath.basename i) i acc
      | false ->
          if exclude (Fpath.basename i) i then acc else
          let i = Fpath.strip_dir_sep i in
          if Fpath.Set.mem i seen then acc else
          let acc = Fpath.Set.add i seen, by_ext in
          if not (Os.Dir.exists i |> Result.error_to_failure) then acc else
          let prune_dir _ dname dir (seen, _) =
            exclude dname dir || Fpath.Set.mem dir seen
          in
          Os.Dir.fold ~dotfiles:true ~prune_dir ~recurse:true
            add i acc |> Result.error_to_failure
    in
    let srcs_i = Fpath.Set.elements srcs_i in
    let acc = Fpath.Set.empty, String.Map.empty in
    try Ok (snd (List.fold_left add_i acc srcs_i)) with
    | Failure e -> Fmt.error "collecting source files: %s" e

  (* Configuration *)

  type t =
    { action_args : string list;
      background : bool;
      b0_dir : Fpath.t;
      brzo_file : Fpath.t option;
      cache_dir : Fpath.t;
      cwd : Fpath.t;
      domain_name : string option;
      domain_confs : domain list;
      hash_fun : (module Hash.T);
      jobs : int;
      log_file : Fpath.t;
      log_level : Log.level;
      memo : (B0_memo.t, string) result Lazy.t;
      no_pager : bool;
      outcome_mode : outcome_mode;
      output_outcome_path : bool;
      pdf_viewer : Cmd.t option;
      root : Fpath.t;
      srcs : (B0_file_exts.map, string) result Lazy.t;
      srcs_i : Fpath.Set.t;
      srcs_x : Fpath.Set.t;
      tty_cap : Tty.cap;
      web_browser : Cmd.t option; }

  let v
      ~action_args ~background ~b0_dir ~brzo_file ~cache_dir ~cwd ~domain_name
      ~domain_confs ~hash_fun ~jobs ~log_file ~log_level ~no_pager
      ~outcome_mode ~output_outcome_path ~pdf_viewer ~root ~srcs_i ~srcs_x
      ~tty_cap ~web_browser ()
    =
    let trash_dir = Fpath.(b0_dir / B0_cli.Memo.trash_dir_name) in
    let srcs = lazy (collect_srcs ~srcs_i ~srcs_x) in
    let memo = lazy (Memo.create ~hash_fun ~cwd ~cache_dir ~trash_dir ~jobs) in
    { action_args; background; b0_dir; brzo_file; cache_dir; cwd; domain_name;
      domain_confs; hash_fun; jobs; log_file; log_level; memo; no_pager;
      outcome_mode; output_outcome_path; pdf_viewer; root; srcs_i; srcs_x;
      srcs; tty_cap; web_browser; }

  let action_args c = c.action_args
  let background c = c.background
  let b0_dir c = c.b0_dir
  let brzo_file c = c.brzo_file
  let cache_dir c = c.cache_dir
  let cwd c = c.cwd
  let domain_name c = c.domain_name
  let domain_confs c = c.domain_confs
  let domain_conf (type a) c (module D : Pre_domain.T with type Conf.t = a) =
    let rec find : domain list -> (string * a) option = function
    | [] -> None
    | Domain ((module D'), outcome, v) :: confs ->
        match Type.Id.provably_equal D.Conf.tid D'.Conf.tid with
        | None -> find confs | Some Type.Equal -> Some (outcome, v)
    in
    find c.domain_confs

  let hash_fun c = c.hash_fun
  let jobs c = c.jobs
  let log_file c = c.log_file
  let log_level c = c.log_level
  let memo c = Lazy.force c.memo
  let outcome_mode c = c.outcome_mode
  let output_outcome_path c = c.output_outcome_path
  let no_pager c = c.no_pager
  let pdf_viewer c = c.pdf_viewer
  let root c = c.root
  let srcs c = Lazy.force c.srcs
  let srcs_i c = c.srcs_i
  let srcs_x c = c.srcs_x
  let tty_cap c = c.tty_cap
  let web_browser c = c.web_browser

  let auto = Fmt.any "<auto>"
  let pp_auto pp = Fmt.option ~none:auto pp
  let pp_hash_fun ppf (module H : Hash.T) = Fmt.string ppf H.id
  let pp =
    Fmt.record
      [ Fmt.field "b0-dir" b0_dir Fpath.pp_quoted;
        Fmt.field "background" background Fmt.bool;
        Fmt.field "brzo-file" brzo_file (pp_auto Fpath.pp_quoted);
        Fmt.field "cache-dir" cache_dir Fpath.pp_quoted;
        Fmt.field "cwd" cwd Fpath.pp_quoted;
        Fmt.field "domain" domain_name (pp_auto Fmt.string);
        Fmt.field "hash-fun" hash_fun pp_hash_fun;
        Fmt.field "jobs" jobs Fmt.int;
        Fmt.field "log-file" log_file Fpath.pp_quoted;
        Fmt.field "no-pager" no_pager Fmt.bool;
        Fmt.field "outcome-mode" outcome_mode pp_outcome_mode;
        Fmt.field "output-outcome-path" output_outcome_path Fmt.bool;
        Fmt.field "pdf-viewer" pdf_viewer (pp_auto Cmd.pp);
        Fmt.field "root" root Fpath.pp_quoted;
        Fmt.field "srcs-i" srcs_i (Fmt.vbox Fpath.(Set.pp pp_quoted));
        Fmt.field "srcs-x" srcs_x (Fmt.vbox Fpath.(Set.pp pp_quoted));
        Fmt.field "web-browser" web_browser (pp_auto Cmd.pp); ]

  let pp_show ppf c = Fmt.pf ppf "@[<v>%a@,%a@]" Fmt.(code string) "Common" pp c
end

(* Conf setup handles cli and brzo file setup *)

module Conf_setup = struct

  (* Root finding *)

  let err_no_root ~cwd =
    let create = Fmt.str (if Sys.win32 then "type NUL >> %s" else "touch %s") in
    let bold = Fmt.(code string) in
    let red = Fmt.(tty [`Bold; `Fg `Red] string) in
    Fmt.error
      "@[<v>%a: @[<v>No %a file found in %a@,\
       or upwards. To %a from this directory use option %a or@,\
       invoke %a to mark the root.@]@]"
      red "Error" bold Conf.brzo_file_name bold (Fpath.to_string cwd)
      bold "brzo" bold "--root ." bold (create Conf.brzo_file_name)

  let find_root_and_brzo_file ~auto_cwd_root ~cwd ~root ~brzo_file =
    let brzo_file_in ~dir =
      let file = Fpath.(dir / Conf.brzo_file_name) in
      match Os.File.exists file with Ok true -> Some file | _ -> None
    in
    let root = match root with
    | Some root ->
        Result.bind (Os.Path.realpath root) @@ fun root ->
        Ok (Some (root, brzo_file_in ~dir:root))
    | None ->
        let rec loop dir = match brzo_file_in ~dir with
        | Some _ as root_brzo_file -> Some (dir, root_brzo_file)
        | None ->
            if not (Fpath.is_root dir) then loop (Fpath.parent dir) else
            if auto_cwd_root then Some (cwd, None) else None
        in
        Ok (loop cwd)
    in
    Result.bind root @@ function
    | None -> err_no_root ~cwd
    | Some (root, root_brzo_file) ->
        let brzo_file = match brzo_file with
        | Some f -> Some Fpath.(cwd // f)
        | None -> root_brzo_file
        in
        Ok (Fpath.add_dir_sep root, brzo_file)

  (* BRZO file *)

  let read_brzo_file ~use_brzo_file file =
    let none = Ok (B0_sexp.Sexp.list [], []) in
    match file with
    | None -> none
    | Some _ when not use_brzo_file -> none
    | Some file ->
        match Sexp.of_file file with
        | Ok _ as v -> v
        | Error e ->
            Log.err (fun m -> m ~header:"" "%s" e);
            Log.warn (fun m -> m "%s file ignored" Conf.brzo_file_name);
            none

  type conf_with_cli =
    (* This holds the parse and cli merge function that was lifted by
       [Pre_domain.T.Conf.with_cli] *)
      Conf_with_cli :
        ((module Pre_domain.T with type Conf.t = 'a) *
         ((B0_sexp.Sexp.t * B0_sexp.Sexpq.path) ->
          ('a, string) result)) -> conf_with_cli

  let parse_with_fun
      (type a) ~outcome_name (module D : Pre_domain.T with type Conf.t = a)
      parse sexp
    =
    let absent = B0_sexp.Sexp.list [], [] in
    let domain_dictq = Sexpq.(key D.name ~absent sexp_with_path) in
    let outcome_key = "outcome" in
    let validate = Some (String.Set.add outcome_key D.Conf.keys) in
    let validateq = Sexpq.key_dom ~validate in
    let onames = Pre_domain.outcome_names (Pre_domain.V (module D)) in
    let outcomeq = Sexpq.(some (atomic (enum ~kind:outcome_key onames))) in
    let outcomeq = Sexpq.key outcome_key ~absent:None outcomeq in
    Result.bind (Sexp.query domain_dictq sexp) @@ fun dict ->
    Result.bind (Sexp.query validateq dict) @@ fun _ ->
    Result.bind (Sexp.query outcomeq dict) @@ fun outcome_conf ->
    let outcome = match outcome_name with
    | Some o -> o
    | None -> Option.value ~default:"exec" outcome_conf
    in
    Result.bind (parse dict) @@ fun c ->
    Ok (Conf.Domain ((module D), outcome, c))

  let parse_domain_conf ~outcome_name sexp (Pre_domain.V (module D)) =
    parse_with_fun ~outcome_name (module D) D.Conf.parse sexp

  let parse_domain_confs ~outcome_name ~all_domains ~domain sexp =
    match domain with
    | None -> List.rev_map (parse_domain_conf ~outcome_name sexp) all_domains
    | Some (Conf_with_cli ((module D), cli_parse)) ->
        let dom = parse_with_fun ~outcome_name (module D) cli_parse sexp in
        let rec loop acc = function
        | [] -> acc
        | (Pre_domain.V (module D') as dv) :: ds ->
            match Type.Id.provably_equal D'.Conf.tid D.Conf.tid with
            | Some Type.Equal -> loop (dom :: acc) ds
            | None ->
                loop (parse_domain_conf ~outcome_name:None sexp dv :: acc) ds
        in
        loop [] all_domains

  let parse_brzo_file ~common_keys ~all_domains ~domain ~outcome_name sexp =
    let doms = List.map Pre_domain.name all_domains in
    let keys = List.rev_append doms common_keys in
    let validate = Some (String.Set.of_list keys) in
    Result.bind (Sexp.query (Sexpq.key_dom ~validate) sexp) @@ fun _ ->
    let results = parse_domain_confs ~outcome_name ~all_domains ~domain sexp in
    let result acc r = match acc, r with
    | Error e, Error e' -> Error (String.concat "\n" [e; e'])
    | (Error _ as e), _ -> e
    | Ok _, (Error _ as e) -> e
    | Ok acc, Ok a -> Ok (a :: acc)
    in
    List.fold_left result (Ok []) results

  let domain_key = "domain"
  let get_domain_name ~all_domains ~domain sexp =
    let name = function (Conf_with_cli ((module D), _)) -> D.name in
    match Option.map name domain with
    | Some _ as d -> Ok d
    | None ->
        let kind = domain_key in
        let domains =
          let add acc d = String.Set.add (Pre_domain.name d) acc in
          List.fold_left add String.Set.empty all_domains
        in
        let domain = Sexpq.(some (atomic (enum ~kind domains))) in
        let q = Sexpq.key domain_key ~absent:None domain in
        Sexp.query q sexp

  let outcome_mode_key = "outcome-mode"
  let outcome_mode_q =
    let kind = outcome_mode_key in
    let modes =
      String.Map.of_list
        [ "action", `Action; "build", `Build; "conf", `Conf; "delete", `Delete;
          "normal", `Normal; "show-path", `Path; ]
    in
    let mode = Sexpq.(atomic (enum_map ~kind modes)) in
    Sexpq.key outcome_mode_key ~absent:`Normal mode

  let get_outcome_mode ~outcome_mode sexp = match outcome_mode with
  | Some m -> Ok m | None -> Sexp.query outcome_mode_q sexp

  let srcs_i_key = "srcs-i"
  let srcs_x_key = "srcs-x"
  let srcs_i_q = Sexpq.(key srcs_i_key ~absent:[] (list Sexp.fpath))
  let srcs_x_q = Sexpq.(key srcs_x_key ~absent:[] (list Sexp.fpath))
  let get_srcs_ix ~root ~srcs_i:i_cli ~srcs_x:x_cli sexp =
    let set_of_list ps =
      let add_path ~root acc f = match Fpath.is_current_dir f with
      | true -> Fpath.Set.add root acc
      | false -> Fpath.Set.add Fpath.(root // f) acc
      in
      List.fold_left (add_path ~root) Fpath.Set.empty ps
    in
    Result.bind (Sexp.query srcs_i_q sexp) @@ fun i_conf ->
    Result.bind (Sexp.query srcs_x_q sexp) @@ fun x_conf ->
    let i_cli = set_of_list i_cli and i_conf = set_of_list i_conf in
    let x_cli = set_of_list x_cli and x_conf = set_of_list x_conf in
    let srcs_i = match Fpath.Set.(is_empty i_conf && is_empty i_cli) with
    | true -> Fpath.Set.singleton root
    | false -> Fpath.Set.(union (diff i_conf x_cli) i_cli)
    in
    let srcs_x = Fpath.Set.(union (diff x_conf i_cli) x_cli) in
    Ok (srcs_i, srcs_x)

  let common_keys = [domain_key; outcome_mode_key; srcs_i_key; srcs_x_key]

  let get_log_file ~cwd ~b0_dir ~log_file = match log_file with
  | None -> Fpath.(b0_dir / Conf.brzo_dir_name / B0_cli.Memo.log_file_name)
  | Some p -> Fpath.(cwd // p)

  let with_cli
      ~auto_cwd_root ~use_brzo_file ~action_args ~background ~b0_dir
      ~brzo_file ~cache_dir ~cwd ~hash_fun ~jobs ~log_file ~log_level ~no_pager
      ~outcome_name ~outcome_mode ~output_outcome_path
      ~pdf_viewer ~root ~srcs_i ~srcs_x ~tty_cap ~web_browser ~all_domains
      ~domain ()
    =
    let tty_cap = B0_cli.B0_std.get_tty_cap tty_cap in
    let log_level = B0_cli.B0_std.get_log_level log_level in
    B0_cli.B0_std.setup tty_cap log_level ~log_spawns:Log.Debug;
    let set_cwd = match cwd with None -> Ok () | Some c -> Os.Dir.set_cwd c in
    Result.bind set_cwd @@ fun () ->
    Result.bind (Os.Dir.cwd ()) @@ fun cwd ->
    Result.bind (find_root_and_brzo_file ~auto_cwd_root ~cwd ~root ~brzo_file)
    @@ fun (root, brzo_file) ->
    Result.bind (read_brzo_file ~use_brzo_file brzo_file) @@ fun sexp ->
    Result.bind
      (parse_brzo_file ~common_keys ~all_domains ~domain ~outcome_name sexp) @@
    fun domain_confs ->
    Result.bind (get_domain_name ~all_domains ~domain sexp) @@
    fun domain_name ->
    Result.bind (get_outcome_mode ~outcome_mode sexp) @@ fun outcome_mode ->
    Result.bind (get_srcs_ix ~root ~srcs_i ~srcs_x sexp) @@
    fun (srcs_i, srcs_x) ->
    let b0_dir = B0_cli.Memo.get_b0_dir ~cwd ~root ~b0_dir in
    let cache_dir = B0_cli.Memo.get_cache_dir ~cwd ~b0_dir ~cache_dir in
    let log_file = get_log_file ~cwd ~b0_dir ~log_file in
    let hash_fun = B0_cli.Memo.get_hash_fun ~hash_fun in
    let jobs = B0_cli.Memo.get_jobs ~jobs in
    Ok (Conf.v ~action_args ~background ~b0_dir ~brzo_file ~cache_dir ~cwd
          ~domain_name ~domain_confs ~hash_fun ~jobs ~log_file ~log_level
          ~no_pager ~outcome_mode ~output_outcome_path ~pdf_viewer ~root
          ~srcs_i ~srcs_x ~tty_cap ~web_browser ())
end

module Cli = struct
  open Cmdliner

  (* Manual fragments *)

  let s_domains = "DOMAIN COMMANDS"
  let s_outcomes = "OUTCOMES"
  let s_outcome_mode = "OUTCOME MODE"
  let s_outcome_opts = "OUTCOME OPTIONS"
  let man_see_manual = `Blocks
      [ `S Manpage.s_see_also;
        `P "Consult $(b,odig doc brzo) for a tutorial and more details."]

  (* General configuration cli arguments *)

  let fpath = B0_cli.fpath
  let docs = Manpage.s_common_options

  let action_args =
    let doc =
      "Arguments given as is to the outcome action. Start with $(b,--) if \
       you need to specify command line options."
    in
    Arg.(value & pos_all string [] & info [] ~doc ~docv:"ARG")

  let b0_dir = B0_cli.Memo.b0_dir ()

  let brzo_file =
    let doc =
      Fmt.str "Use $(docv) for the BRZO file. Use with $(b,%a) to disable \
               a BRZO file present at the brzo root. Note that this option \
               does not affect root finding (see $(b,--root)), it only \
               replaces the BRZO file to use."
        Fpath.pp_quoted Fpath.null
    in
    let docv = "FILE" in
    let env = Cmd.Env.info "BRZO_FILE" in
    let absent = "$(b,BRZO) file at the brzo root" in
    Arg.(value & opt (some fpath) None &
         info ["brzo-file"] ~absent ~env ~doc ~docv ~docs)

  let background = B0_web_browser.background ~docs ()
  let web_browser = B0_web_browser.browser ~opts:["browser"] ~docs ()
  let cache_dir = B0_cli.Memo.cache_dir ()
  let cwd =
    let doc =
      "Set the current working directory to $(docv) before doing anything. \
       Relative file path on the command line are interpreted relative to this \
       new cwd."
    in
    Arg.(value & opt (some fpath) None & info ["C"] ~doc ~docv:"DIR" ~docs)

  let outcome_mode =
    let docs = s_outcome_mode in
    let m symb args doc = (Some symb), Arg.info args ~doc ~docs in
    let modes =
      [ m `Action ["r"; "run-action"] "Only perform the outcome action.";
        m `Build ["b"; "build"] "Only build the outcome artefact.";
        m `Conf ["conf"] "Only show the configuration.";
        m `Delete ["d"; "delete"] "Delete the outcome build.";
        m `Normal ["a"; "normal"]
          "Build outcome and perform the action (default).";
        m `Path ["show-path"] "Only show the path to the outcome artefact." ]
    in
    Arg.(value & vflag None modes)

  let output_outcome_path =
    let docs = s_outcome_mode in
    let doc =
      "On successful build with $(b,-b), output the outcome artefact path on \
       $(b,stdout). For example for timing an outcome execution without \
       timing the build: $(b,time \\$(brzo -b --path)) $(i,ARG)…"
    in
    Arg.(value & flag & info ["path"] ~doc ~docs)

  let root =
    let doc =
      "Use $(docv) as the brzo root. If unspecified use the first directory, \
       from the current working directory and moving upwards that has a \
       BRZO file."
    in
    let docv = "DIR" in
    let env = Cmd.Env.info "BRZO_ROOT" in
    let none = "automatically determined" in
    Arg.(value & opt (some ~none fpath) None &
         info ["root"] ~env ~doc ~docv ~docs)

  let hash_fun =
    B0_cli.Memo.hash_fun ~docs ~env:(Cmd.Env.info "BRZO_HASH_FUN") ()

  let jobs = B0_cli.Memo.jobs ~docs ~env:(Cmd.Env.info "BRZO_JOBS") ()
  let log_file =
    let env = Cmd.Env.info "BRZO_LOG_FILE" in
    let doc_none = "$(b,.log) in $(b,brzo) directory of b0 directory" in
    B0_cli.Memo.log_file ~docs ~doc_none ~env ()

  let log_level =
    B0_cli.B0_std.log_level ~docs ~env:(Cmd.Env.info "BRZO_VERBOSITY") ()

  let no_pager = B0_pager.don't ~docs ()
  let pdf_viewer = B0_pdf_viewer.pdf_viewer ~docs ()

  let srcs_i =
    let doc =
      "Include source files which have $(docv) as a prefix; repeatable. \
       Also removes $(docv) from excludes specified in the $(b,srcs-x) key of \
       the BRZO file. If unspecified, this is the brzo root directory.
       Relative file paths are expressed relative to the brzo root directory."
    in
    let docv = "PATH" in
    Arg.(value & opt_all fpath [] & info ["i"; "srcs-i"] ~doc ~docv ~docs)

  let srcs_x =
    let doc =
      "Exclude source files which have $(docv) as a prefix; repeatable. \
       Also removes $(docv) from includes specified in the $(b,srcs-i) key of
       the BRZO file. Paths with dotfile, $(b,_b0), or $(b,_build) segments
       are automatically excluded, this can be overriden by specifying them
       explicitely via $(b,-i). Relative file paths are expressed relative to
       the brzo root directory."
    in
    let docv = "PATH" in
    Arg.(value & opt_all fpath [] & info ["x"; "srcs-x"] ~doc ~docv ~docs)

  let tty_cap =
    B0_cli.B0_std.tty_cap ~docs ~env:(Cmd.Env.info "BRZO_COLOR") ()

  (* Domain specific cli *)

  let tnone = Term.const None
  let outcome_name = function
  | None -> tnone
  | Some (Pre_domain.V v) ->
    let module D = (val v : Pre_domain.T with type Conf.t = 'a) in
    let add acc o =
      let name = Pre_domain.outcome_name o and doc = Pre_domain.outcome_doc o in
      let docs = s_outcomes in
      (Some name, Arg.info [Fmt.str "%s" name] ~doc ~docs) :: acc
    in
    Arg.value (Arg.vflag None (List.fold_left add [] D.pre_outcomes))

  let outcome_mode = function None -> tnone | Some _ -> outcome_mode
  let output_outcome_path = function
  | None -> Term.const false | Some _ -> output_outcome_path

  let action_args = function None -> Term.const [] | Some _ -> action_args
  let domain_cli_conf = function
  | None -> tnone
  | Some (Pre_domain.V (module D)) when D.name = "default" -> tnone
  | Some (Pre_domain.V (module D)) ->
      let conf parser = Some (Conf_setup.Conf_with_cli ((module D), parser)) in
      Term.(const conf $ D.Conf.parse_with_cli)

  (* Configuration *)

  let conf ~auto_cwd_root ~use_brzo_file ~domain ~all_domains =
    let c
        action_args b0_dir background brzo_file cache_dir cwd hash_fun jobs
        log_file log_level no_pager outcome_name outcome_mode
        output_outcome_path pdf_viewer root srcs_i srcs_x tty_cap web_browser
        domain
      =
      Result.map_error (fun s -> `Msg s) @@
      Conf_setup.with_cli
        ~auto_cwd_root ~use_brzo_file ~action_args ~background ~b0_dir
        ~brzo_file ~cache_dir ~cwd ~hash_fun ~jobs ~log_file ~log_level
        ~no_pager ~outcome_name ~outcome_mode ~output_outcome_path ~pdf_viewer
        ~root ~srcs_i ~srcs_x ~tty_cap ~web_browser ~all_domains ~domain ()
    in
    let outcome_name = outcome_name domain in
    let outcome_mode = outcome_mode domain in
    let output_outcome_path = output_outcome_path domain in
    let action_args = action_args domain in
    let domain_cli_conf = domain_cli_conf domain in
    Term.term_result @@
    Term.(const c $ action_args $ b0_dir $ background $ brzo_file $ cache_dir $
          cwd $ hash_fun $ jobs $ log_file $ log_level $ no_pager $
          outcome_name $ outcome_mode $ output_outcome_path $
          pdf_viewer $ root $ srcs_i $ srcs_x $ tty_cap $ web_browser $
          domain_cli_conf)
end

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
