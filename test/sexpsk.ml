(*---------------------------------------------------------------------------
   Copyright (c) 2019 The serialk programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Serialk_sexp

module File = struct

  (* Booooring *)

  let catch_sys_error fn = try fn () with Sys_error e -> Error e

  let with_io_chan close file chan fn =
    try let r = fn chan in close chan; Ok r with
    | e ->
        (try ignore (close chan) with Sys_error _ -> ());
        match e with
        | Sys_error err -> Error (Printf.sprintf "%s: %s" file err)
        | End_of_file ->
            Error (Printf.sprintf "%s: unexpected end of file" file)
        | e -> raise e

  let with_open_in file fn =
    catch_sys_error @@ fun () ->
    let ic = if file = "-" then stdin else open_in_bin file in
    let close_in ic = if file = "-" then () else close_in ic in
    with_io_chan close_in file ic fn

  let read file =
    with_open_in file @@ fun ic ->
    let bsize = 65536 (* IO_BUFFER_SIZE *) in
    let buf = Buffer.create bsize in
    let b = Bytes.create bsize in
    let rec loop () =
      let rc = input ic b 0 bsize in
      if rc = 0 then Buffer.contents buf else
      (Buffer.add_subbytes buf b 0 rc; loop ())
    in
    loop ()
end

let err_file = 1
let err_sexp = 2
let err_query = 3

let log_err fmt =
  Format.fprintf Format.err_formatter
    ("%s: @[" ^^ fmt ^^ "@]@.") (Filename.basename Sys.executable_name)

let log_on_error ~exit:code r f = match r with
| Error e -> log_err "%s" e; code
| Ok v -> f v

let delete file path =
  log_on_error ~exit:err_file (File.read file) @@ fun content ->
  log_on_error ~exit:err_sexp (Sexp.seq_of_string ~file content) @@ fun sexp ->
  log_on_error ~exit:err_query (Sexpq.query (Sexpq.path path Sexpq.sexp) sexp)
  @@ fun sexp -> Format.printf "@[%a@]@." Sexp.pp_seq sexp; 0

let get file path =
  log_on_error ~exit:err_file (File.read file) @@ fun content ->
  log_on_error ~exit:err_sexp (Sexp.seq_of_string ~file content) @@ fun sexp ->
  match path with
  | None -> Format.printf "@[%a@]@." Sexp.pp_seq sexp; 0
  | Some path ->
      let result = Sexpq.query (Sexpq.path path Sexpq.sexp) sexp in
      log_on_error ~exit:err_query result @@ fun result ->
      match result with
      | `A (a, _) | `L ([`A (a, _)], _) -> Format.printf "%s@." a; 0
      | `L _ as l -> Format.printf "@[%a@]@." Sexp.pp l; 0

let locs file =
  log_on_error ~exit:err_file (File.read file) @@ fun content ->
  log_on_error ~exit:err_sexp (Sexp.seq_of_string ~file content) @@ fun sexp ->
  let pp_loc ppf l = Serialk_text.Tloc.pp ppf l; Format.pp_print_char ppf ':' in
  let rec pp_locs ppf = function
  | `A (_, _) as s -> pp_loc ppf (Sexp.loc s)
  | `L (vs, _) as s ->
      pp_loc ppf (Sexp.loc s); Format.pp_print_cut ppf ();
      Format.pp_print_list pp_locs ppf vs
  in
  Format.printf "@[<v>%a@]@." pp_locs sexp; 0

let set file caret v =
  log_on_error ~exit:err_file (File.read file) @@ fun content ->
  log_on_error ~exit:err_sexp (Sexp.seq_of_string ~file content) @@ fun sexp ->
  log_on_error ~exit:err_sexp (Sexp.seq_of_string v) @@ fun v ->
  let p = Sexp.caret_path caret in
  log_on_error ~exit:err_query (Sexpq.query (Sexpq.probe_path p) sexp) @@
  fun (loc, miss) ->
  let rep = Sexp.seq_pave_caret (Sexp.repath_caret miss caret) v in
  let rep = Format.asprintf "%a" Sexp.pp_seq rep in
  let start, stop = match caret with
  | Before _ -> let s = Serialk_text.Tloc.sbyte loc in s, s
  | Over _ when miss <> [] -> let s = Serialk_text.Tloc.sbyte loc in s, s
  | Over _ -> Serialk_text.Tloc.sbyte loc, Serialk_text.Tloc.ebyte loc + 1
  | After _ -> let e = Serialk_text.Tloc.ebyte loc + 1 in e, e
  in
  let res = Serialk_text.Tloc.string_replace ~start ~stop ~rep content in
  Format.printf "%s" res; 0

(* Command line interface *)

open Cmdliner

let exits =
  Term.exit_info
    err_file ~doc:"on file read errors" ::
  Term.exit_info
    err_sexp ~doc:"on s-expression parse errors" ::
  Term.exit_info
    err_query ~doc:"on path query errors" ::
  Term.default_exits

let common_man =
  [ `S Manpage.s_bugs;
    `P "This program is distributed with the serialk OCaml library.
     See $(i,%%PKG_HOMEPAGE%%) for contact information."; ]

let file_arg =
  let doc = "$(docv) is the s-expression file. Use $(b,-) for stdin." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"FILE")

let path_arg =
  let parse s = match Sexp.path_of_string s with
  | Ok _ as v -> v | Error e -> Error (`Msg e)
  in
  let pp = Sexp.pp_path in
  Arg.conv ~docv:"PATH" (parse, pp)

let caret_arg =
  let parse s = match Sexp.caret_of_string s with
  | Ok _ as v -> v | Error e -> Error (`Msg e)
  in
  let pp = Sexp.pp_caret in
  Arg.conv ~docv:"CARET" (parse, pp)

let delete_cmd =
  let doc = "Delete an s-expression path" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) returns the value of an s-expression file key.";
    `S Manpage.s_examples;
    `P "TODO";
    `Blocks common_man; ]
  in
  let path_arg =
    let doc = "Delete s-expression path $(docv)." in
    Arg.(required & pos 1 (some path_arg) None & info [] ~doc ~docv:"PATH")
  in
  Term.(const delete $ file_arg $ path_arg),
  Term.info "delete" ~doc ~sdocs ~exits ~man

let get_cmd =
  let doc = "Extract an s-expression path (default)" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) returns the value of an s-expression file key.";
    `S Manpage.s_examples;
    `P "TODO";
    `Blocks common_man; ]
  in
  let key_opt_arg =
    let doc = "Extract s-expression path $(docv)." in
    Arg.(value & pos 1 (some path_arg) None & info [] ~doc ~docv:"PATH")
  in
  Term.(const get $ file_arg $ key_opt_arg),
  Term.info "get" ~doc ~sdocs ~exits ~man

let set_cmd =
  let doc = "Edit an s-expression path" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) sets the value of an s-expression file key.";
    `S Manpage.s_examples;
    `P "TODO";
    `Blocks common_man; ]
  in
  let caret_arg =
    let doc = "Set caret $(docv)." in
    Arg.(required & pos 1 (some caret_arg) None & info [] ~doc ~docv:"CARET")
  in
  let sexp =
    let doc = "$(docv) to insert or substitute" in
    Arg.(required & pos 2 (some string) None & info [] ~doc ~docv:"SEXP")
  in
  Term.(const set $ file_arg $ caret_arg $ sexp),
  Term.info "set" ~doc ~sdocs ~exits ~man

let locs_cmd =
  let doc = "Show s-expression parse locations" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) outputs s-expression parse locations.";
    `Blocks common_man; ]
  in
  Term.(const locs $ file_arg),
  Term.info "locs" ~doc ~sdocs ~exits ~man

let cmds = [delete_cmd; get_cmd; locs_cmd; set_cmd;]

let default =
  let doc = "Process s-expressions" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(mname) edits s-expression files";
    `S Manpage.s_examples;
    `S Manpage.s_bugs;
    `P "This program is distributed with the serialk OCaml library.
        See $(i,%%PKG_HOMEPAGE%%) for contact information."; ]
  in
  fst (List.hd cmds),
  Term.info "sexpsk" ~version:"%%VERSION%%" ~doc ~sdocs ~exits ~man

let () = Term.(exit @@ eval_choice default cmds)

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The serialk programmers

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
