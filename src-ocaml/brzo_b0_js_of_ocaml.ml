(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

module Js_of_ocaml = struct
  let env_vars = [ "BUILD_PATH_PREFIX_MAP" ]
  let tool = B0_memo.Tool.by_name ~vars:env_vars "js_of_ocaml"

  let link_tool = B0_memo.Tool.by_name ~vars:env_vars "jsoo_link"

  let compile ?args:(more_args = Cmd.empty) m ~byte_exe ~o =
    let js_of_ocaml = B0_memo.tool m tool in
    B0_memo.spawn m ~reads:[byte_exe] ~writes:[o] @@
    js_of_ocaml Cmd.(arg "-o" %% (path o) %% more_args %% path byte_exe)

  let compile_toplevel ?args:(more_args = Cmd.empty) m ~byte_exe ~mod_names ~o =
    let js_of_ocaml = B0_memo.tool m tool in
    B0_memo.spawn m ~reads:[byte_exe; mod_names] ~writes:[o] @@
    js_of_ocaml Cmd.(arg "-o" %% (path o) %% more_args %
                     "--toplevel" % "--export" %% unstamp (path mod_names) %
                     "+toplevel.js" % "+dynlink.js" %%
                     unstamp (path byte_exe))

  let link ?args:(more_args = Cmd.empty) m ~jss ~o =
    let jsoo_link = B0_memo.tool m link_tool in
    B0_memo.spawn m ~reads:jss ~writes:[o] @@
    jsoo_link Cmd.(arg "-o" %% (path o) %% more_args %% paths jss)

  let js_escape =
    let char_len = function '\\' | '"' | '\n' | '\r' -> 2 | _ -> 1 in
    let set_char b i = function
    | '\\' -> Bytes.set b i '\\'; Bytes.set b (i + 1) '\\'; i + 2
    | '"' -> Bytes.set b i '\\'; Bytes.set b (i + 1) '"'; i + 2
    | '\n' -> Bytes.set b i '\\'; Bytes.set b (i + 1) 'n'; i + 2
    | '\r' -> Bytes.set b i '\\'; Bytes.set b (i + 1) 'r'; i + 2
    | c -> Bytes.set b i c; i + 1
    in
    String.byte_escaper char_len set_char

  let js_array ~buf s = match s with
  | "" -> "[]"
  | s ->
      let add_byte buf byte =
        Buffer.add_string buf "0x";
        Buffer.add_char buf (Char.Ascii.lower_hex_digit_of_int (byte lsr 4));
        Buffer.add_char buf (Char.Ascii.lower_hex_digit_of_int (byte      ));
      in
      Buffer.reset buf;
      Buffer.add_char buf '[';
      add_byte buf (Char.code s.[0]);
      for i = 1 to (String.length s - 1)
      do Buffer.add_char buf ','; add_byte buf (Char.code s.[i]) done;
      Buffer.add_char buf ']';
      Buffer.contents buf

  let js_fs_tmp_files ~cwd ~args =
    let js_file ~buf n c =
      Fmt.str "{name:\"%s\", content: %s}" (js_escape n) (js_array ~buf c)
    in
    let buf = Buffer.create 255 in
    let add_file_arg acc a = match Fpath.of_string a with
    | Error _ -> acc
    | Ok a ->
        let file = Fpath.(cwd // a) in
        match Os.File.exists file |> Result.value ~default:false with
        | false -> acc
        | true ->
            match Os.File.read file with
            | Error e -> Log.err (fun m -> m "%s" e); acc
            | Ok c -> js_file ~buf (Fpath.to_string file) c :: acc
    in
    List.fold_left add_file_arg [] args

  (* See caml_sys_{getenv,get_argv} in jsoo's stdlib.js. *)
  let tty_glue ~exe ~cwd ~env ~args =
    let files = js_fs_tmp_files ~cwd ~args in
    let cwd = js_escape (Fpath.to_string cwd) in
    let env =
      let env_var k v = Fmt.str "\"%s\": \"%s\"" (js_escape k) (js_escape v) in
      let add_var k v acc = env_var k v :: acc in
      let env = List.rev (Os.Env.fold add_var env []) in
      env_var "BRZO_HTML" "true" :: env
    in
    let args =
      let cli_arg a = Fmt.str "\"%s\"" (js_escape a) in
      let exe = cli_arg (Fpath.to_string exe) in
      exe :: exe :: List.map cli_arg args
    in
    Fmt.str "(function () {\n\
            \  window.process = {\n\
            \    cwd: function () { return \"%s\"; },\n\
            \    env: {%s},\n\
            \    argv: [%s]};\n\
            \  window.caml_fs_tmp = [%s];
            }());"
      cwd (String.concat ", " env) (String.concat ", " args)
      (String.concat ", " files)


  let ocaml_toplevel_css =
{css|
body { background: #181B20; color: #8C8D90; }
#ocaml { margin:0; padding: 1.5rem; }
#ocaml * { background: inherit; color: inherit;
           margin:0; padding:0; border: none; outline: none; resize:none;
           font-family:monospace,monospace; font-size:1rem; }
#ocaml pre { margin-bottom:0.3rem; }
#ocaml div * { display: inline-block; vertical-align: text-top; }
#ocaml div span:first-child { color: #00D900 }
|css}

  let write_page
      ?(toplevel_css = false) ?(generator = "") ?(lang = "") ?(scripts = [])
      ?(styles = []) ?(title = "") m ~o
    =
    let title = if title = ""
      then Fpath.basename ~strip_exts:true o else title in
    let stamp =
      String.concat ""
        (string_of_bool toplevel_css :: generator :: lang :: title ::
         List.rev_append styles scripts)
    in
    B0_memo.write m ~stamp o @@ fun () ->
    let open B0_html in
    let body =
      let sorry = "Sorry, you need to enable JavaScript to see this page." in
      El.(body [noscript [txt sorry]])
    in
    let more_head = match toplevel_css with
    | true -> El.style ~at:At.[type' "text/css"] [El.txt ocaml_toplevel_css]
    | false -> El.void
    in
    let page =
      El.page ~generator ~lang ~scripts ~styles ~title ~more_head body
    in
    Ok (El.to_string ~doctype:true page)

  let toplevel_ui_src =
{ocaml|
open Js_of_ocaml_toplevel
open Jsoo_runtime

let () = (* Don't pollute the toplevel namespace *)
  let module History = struct
    type t = { prev : string list; focus : string; next : string list; }
    let v prev =
      let add acc e = match String.trim e with "" -> acc | e -> e :: acc in
      { prev = List.rev (List.fold_left add [] prev) ; focus = ""; next = [] }

    let empty = v []
    let push e es =
      if e = "" then es else match es with
      | e' :: _ when String.equal e e' -> es
      | es -> e :: es

    let entries h =
      let next = List.filter (fun s -> not (String.equal s "")) h.next in
      List.rev_append (push h.focus next) h.prev

    let add h e = match String.trim e with "" -> h | e -> v (push e (entries h))
    let restart h = v (entries h)
    let prev h current = match h.prev with
    | [] -> None
    | p :: ps ->
        let next = push (String.trim current) (push h.focus h.next) in
        let next = if next = [] then [""] (* bottom can be empty *) else next in
        Some ({ prev = ps; focus = p; next; }, p)

    let next h current = match h.next with
    | [] -> None
    | n :: ns ->
        let prev = push (String.trim current) (push h.focus h.prev) in
        Some ({ prev; focus = n; next = ns }, n)
  end
  in
  let module Toplevel = struct
    let null = Js.pure_js_expr "null"
    let window = Js.pure_js_expr "window"
    let document = Js.pure_js_expr "document"
    let el s = Js.meth_call document "createElement" [| Js.string s |]
    let txt s = Js.meth_call document "createTextNode" [| s |]
    let append_child e c = ignore (Js.meth_call e "appendChild" [|c|])
    let insert_before e b c =  ignore (Js.meth_call e "insertBefore" [|b;c|])
    let add_event_listener e ev cb =
      ignore (Js.meth_call e "addEventListener"
                [|Js.string ev; Js.wrap_callback cb|])

    let execute phrase =
      let buf = Buffer.create 100 in
      let ppf = Format.formatter_of_buffer buf in
      JsooTop.execute true ppf phrase; Buffer.contents buf

    let export_js () = (* to be used in the browser console *)
      let exec s = Js.string (execute (Js.to_string s)) in
      Js.set (Js.pure_js_expr "jsoo_exports")
        (Js.string "ocaml") (Obj.magic exec)

    let history_prev, history_next, history_save =
      let h = ref (History.v []) in
      let history_prev s = match History.prev !h s with
      | None -> s | Some (h', s) -> h := h'; s
      in
      let history_next s = match History.next !h s with
      | None -> s | Some (h', s) -> h := h'; s
      in
      let history_save s =
        let s = match s.[String.length s - 1] = '\n' with
        | true -> String.sub s 0 (String.length s - 1)
        | false -> s
        in
        h := History.add !h s
      in
      history_prev, history_next, history_save

    let handle_text_input i print s =
      let line_count = List.length (String.split_on_char '\n' s) in
      Js.set i (Js.string "rows") (Js.string (string_of_int line_count));
      let len = String.length s in
      if len < 3 then false else
      match s.[len - 3] = ';' && s.[len - 2] = ';'  && s.[len - 1] = '\n' with
      | false -> false
      | true -> history_save s; print ("# " ^ s); print (execute s); true

    let create () =
      let toplevel =
        let t = el "div" in
        let () = Js.set t (Js.string "id") (Js.string "ocaml") in
        t
      in
      let prompt = el "div" in
      let print ~js_string:s =
        let output = el "pre" in
        let () = append_child output (txt s) in
        insert_before toplevel output prompt
      in
      let input =
        let i = el "textarea" in
        let get_i () = Js.to_string (Js.get i (Js.string "value")) in
        let set_i s = Js.set i (Js.string "value") (Js.string s) in
        let oninput _ =
          let print s = print ~js_string:(Js.string s) in
          if handle_text_input i print (get_i ()) then set_i "";
          Js.bool false
        in
        let onkeydown e =
          let get_bool e f = Js.to_bool (Js.get e (Js.string f)) in
          let (key : int) = Obj.magic @@ Js.get e (Js.string "keyCode") in
          match get_bool e "ctrlKey" || get_bool e "metaKey" with
          | false -> Js.bool true
          | true ->
              let f = Js.bool false in
              if key = 38 then (set_i (history_prev (get_i ())); f) else
              if key = 40 then (set_i (history_next (get_i ())); f) else
              Js.bool true
        in
        let () = Js.set i (Js.string "cols") (Js.string "80") in
        let () = Js.set i (Js.string "rows") (Js.string "1") in
        let () = Js.set i (Js.string "autofocus") (Js.string "true") in
        let () = Js.set i (Js.string "spellcheck") null in
        let () = add_event_listener i "input" oninput in
        let () = add_event_listener i "keydown" onkeydown in
        i
      in
      let pchar =
        let span = el "span" in
        append_child span (txt (Js.string "# ")); span
      in
      let () = append_child prompt pchar in
      let () = append_child prompt input in
      let () = append_child toplevel prompt in
      Sys.set_channel_output' stdout print;
      Sys.set_channel_output' stderr print;
      let announce =
        String.concat ""
          [ "        OCaml version "; Stdlib.Sys.ocaml_version;
            " -- courtesy of js_of_ocaml ";
            (* See https://github.com/ocsigen/js_of_ocaml/issues/1236 *)
            Js_of_ocaml.Sys_js.js_of_ocaml_version; ]
      in
      let help = " \nUse {ctrl,meta}-{up,down} for history.\n\n" in
      JsooTop.initialize ();
      print ~js_string:(Js.string announce);
      print ~js_string:(Js.string help);
      toplevel

    let setup () =
      let setup _ =
        let toplevel = create () in
        export_js ();
        append_child (Js.get document (Js.string "body")) toplevel;
        Js.bool false
      in
      add_event_listener window "load" setup
  end
  in
  Toplevel.setup ()
|ocaml}
end
