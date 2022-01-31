(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00

module Js_of_ocaml = struct
  let env_vars = [ "BUILD_PATH_PREFIX_MAP" ]
  let tool = Tool.by_name ~vars:env_vars "js_of_ocaml"

  let link_tool = Tool.by_name ~vars:env_vars "jsoo_link"

  let compile ?args:(more_args = Cmd.empty) m ~byte_exe ~o =
    let js_of_ocaml = Memo.tool m tool in
    Memo.spawn m ~reads:[byte_exe] ~writes:[o] @@
    js_of_ocaml Cmd.(atom "-o" %% (path o) %% more_args %% path byte_exe)

  let compile_toplevel ?args:(more_args = Cmd.empty) m ~byte_exe ~mod_names ~o =
    let js_of_ocaml = Memo.tool m tool in
    Memo.spawn m ~reads:[byte_exe; mod_names] ~writes:[o] @@
    js_of_ocaml Cmd.(atom "-o" %% (path o) %% more_args %
                     "--toplevel" % "--no-runtime" %
                     "--export" %% unstamp (path mod_names) %
                     "+runtime.js" % "+toplevel.js" % "+dynlink.js" %%
                     unstamp (path byte_exe))

  let link ?args:(more_args = Cmd.empty) m ~jss ~o =
    let jsoo_link = Memo.tool m link_tool in
    Memo.spawn m ~reads:jss ~writes:[o] @@
    jsoo_link Cmd.(atom "-o" %% (path o) %% more_args %% paths jss)

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
        Buffer.add_char buf (Char.Ascii.lower_hex_digit (byte lsr 4));
        Buffer.add_char buf (Char.Ascii.lower_hex_digit byte)
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
      let env = List.rev (String.Map.fold add_var env []) in
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
    let title = if title = "" then Fpath.basename ~no_ext:true o else title in
    let stamp =
      String.concat ""
        (string_of_bool toplevel_css :: generator :: lang :: title ::
         List.rev_append styles scripts)
    in
    Memo.write m ~stamp o @@ fun () ->
    let open B00_htmlg in
    let body =
      let sorry = "Sorry, you need to enable JavaScript to see this page." in
      El.(body [noscript [txt sorry]])
    in
    let more_head = match toplevel_css with
    | true -> El.style ~at:At.[type' "text/css"] [El.txt ocaml_toplevel_css]
    | false -> El.void
    in
    let page =
      El.basic_page ~generator ~lang ~scripts ~styles ~title ~more_head body
    in
    Ok (El.to_string ~doc_type:true page)

  let toplevel_ui_src =
{ocaml|
open Js_of_ocaml
open Js_of_ocaml_toplevel

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
    let execute phrase =
      let buf = Buffer.create 100 in
      let ppf = Format.formatter_of_buffer buf in
      JsooTop.execute true ppf phrase; Buffer.contents buf

    let export_js () = (* to be used in the browser console *)
      let exec s = Js.string (execute (Js.to_string s)) in
      Js.export "ocaml" exec

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
      Js.Unsafe.set i "rows" (Js.string (string_of_int line_count));
      let len = String.length s in
      if len < 3 then false else
      match s.[len - 3] = ';' && s.[len - 2] = ';'  && s.[len - 1] = '\n' with
      | false -> false
      | true -> history_save s; print ("# " ^ s); print (execute s); true

    let create () =
      let txt s =
        Js.Unsafe.(meth_call Dom_html.document "createTextNode" [| inject s |])
      in
      let toplevel =
        let t = Dom_html.(createDiv document) in
        let () = Js.Unsafe.set t "id" (Js.string "ocaml") in
        t
      in
      let prompt = Dom_html.(createDiv document) in
      let print s =
        let output = Dom_html.(createPre document) in
        let () = Dom.appendChild output (txt (Js.string s)) in
        Dom.insertBefore toplevel output (Js.Opt.return prompt);
      in
      let input =
        let i = Dom_html.(createTextarea document) in
        let get_i () = Js.to_string (Js.Unsafe.get i "value") in
        let set_i s = Js.Unsafe.set i "value" (Js.string s) in
        let oninput _ =
          if handle_text_input i print (get_i ()) then set_i "";
          Js._false
        in
        let onkeydown e =
          let get_bool e f = Js.to_bool (Js.Unsafe.get e f) in
          let (key : int) = Js.Unsafe.get e "keyCode" in
          match get_bool e "ctrlKey" || get_bool e "metaKey" with
          | false -> Js._true
          | true ->
            if key = 38 then (set_i (history_prev (get_i ())); Js._false) else
            if key = 40 then (set_i (history_next (get_i ())); Js._false) else
            Js._true
        in
        let () = Js.Unsafe.set i "cols" (Js.string "80") in
        let () = Js.Unsafe.set i "rows" (Js.string "1") in
        let () = Js.Unsafe.set i "autofocus" (Js.string "true") in
        let () = Js.Unsafe.set i "spellcheck" Js.null in
        let () = Js.Unsafe.set i "oninput" (Dom_html.handler oninput) in
        let () = Js.Unsafe.set i "onkeydown" (Dom_html.handler onkeydown) in
        i
      in
      let pchar =
        let span = Dom_html.(createSpan document) in
        Dom.appendChild span (txt "# "); span
      in
      let () = Dom.appendChild prompt pchar in
      let () = Dom.appendChild prompt input in
      let () = Dom.appendChild toplevel prompt in
      Sys_js.set_channel_flusher stdout print;
      Sys_js.set_channel_flusher stderr print;
      let announce =
        String.concat ""
          [ "        OCaml version "; Sys.ocaml_version;
            " -- courtesy of js_of_ocaml "; Sys_js.js_of_ocaml_version; ]
      in
      JsooTop.initialize ();
      print announce; print " \nUse {ctrl,meta}-{up,down} for history.\n\n";
      toplevel

  end
  in

  let setup =
    Dom_html.handler @@ fun _ ->
    let toplevel = Toplevel.create () in
    Toplevel.export_js ();
    Dom.appendChild (Js.Unsafe.get Dom_html.document "body") toplevel;
    Js._false
  in
  ignore (Dom_html.addEventListener Dom_html.window
          Dom_html.Event.load setup Js._false)
|ocaml}
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
