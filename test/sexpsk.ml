(*---------------------------------------------------------------------------
   Copyright (c) 2019 The serialk programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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

let exec = Filename.basename Sys.executable_name
let log_err fmt =
  Format.fprintf Format.err_formatter ("%s: @[" ^^ fmt ^^ "@]@.") exec

let log_on_error ~exit:code r f = match r with
| Error e -> log_err "%s" e; code | Ok v -> f v

let delete file path =
  let query = Sexpq.delete_at_path ~must_exist:false path in
  log_on_error ~exit:err_file (File.read file) @@ fun content ->
  log_on_error ~exit:err_sexp (Sexp.seq_of_string' ~file content) @@ fun sexp ->
  log_on_error ~exit:err_query (Sexpq.query' query sexp) @@ fun sexp ->
  Format.printf "@[%a@]" Sexp.pp_seq_layout sexp; 0

let get file path =
  log_on_error ~exit:err_file (File.read file) @@ fun content ->
  log_on_error ~exit:err_sexp (Sexp.seq_of_string' ~file content) @@ fun sexp ->
  match path with
  | None -> Format.printf "@[%a@]" Sexp.pp_seq_layout sexp; 0
  | Some path ->
      let query = Sexpq.path path Sexpq.sexp in
      log_on_error ~exit:err_query (Sexpq.query' query sexp) @@ function
      | `A (a, _) | `L ([`A (a, _)], _) -> Format.printf "%s@." a; 0
      | `L _ as l -> Format.printf "@[%a@]@." Sexp.pp l; 0

let locs file =
  let pp_loc ppf l = Serialk_text.Tloc.pp ppf l; Format.pp_print_char ppf ':' in
  let rec pp_locs ppf = function
  | `A (_, _) as s -> pp_loc ppf (Sexp.loc s)
  | `L (vs, _) as s ->
      pp_loc ppf (Sexp.loc s); Format.pp_print_cut ppf ();
      Format.pp_print_list pp_locs ppf vs
  in
  log_on_error ~exit:err_file (File.read file) @@ fun content ->
  log_on_error ~exit:err_sexp (Sexp.seq_of_string' ~file content) @@ fun sexp ->
  Format.printf "@[<v>%a@]@." pp_locs sexp; 0

let set file caret v =
  log_on_error ~exit:err_file (File.read file) @@ fun content ->
  log_on_error ~exit:err_sexp (Sexp.seq_of_string' ~file content) @@ fun sexp ->
  log_on_error ~exit:err_sexp (Sexp.seq_of_string' v) @@ fun v ->
  let query = Sexpq.splice_at_caret ~must_exist:false caret ~rep:v in
  log_on_error ~exit:err_query (Sexpq.query' query sexp) @@ fun sexp ->
  Format.printf "@[%a@]" Sexp.pp_seq_layout sexp; 0

(* Command line interface *)

open Cmdliner

let file_arg =
  let doc = "$(docv) is the s-expression file. Use $(b,-) for stdin." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"FILE")

let path_arg =
  let parse s = match Sexp.path_of_string s with
  | Ok _ as v -> v | Error e -> Error (`Msg e)
  in
  Arg.conv ~docv:"SPATH" (parse, Sexp.pp_path ())

let caret_arg =
  let parse s = match Sexp.caret_of_string s with
  | Ok _ as v -> v | Error e -> Error (`Msg e)
  in
  Arg.conv ~docv:"CARET" (parse, Sexp.pp_caret ())

let exits =
  Cmd.Exit.info err_file ~doc:"on file read errors" ::
  Cmd.Exit.info err_sexp ~doc:"on s-expression parse errors" ::
  Cmd.Exit.info err_query ~doc:"on path query errors" ::
  Cmd.Exit.defaults

let common_man =
  [ `S Manpage.s_bugs;
    `P "This program is distributed with the serialk OCaml library. \
        See $(i,%%PKG_HOMEPAGE%%) for contact information."; ]

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
    Arg.(required & pos 1 (some path_arg) None & info [] ~doc ~docv:"SPATH")
  in
  Cmd.v (Cmd.info "delete" ~doc ~sdocs ~exits ~man)
    Term.(const delete $ file_arg $ path_arg)

let get_cmd, get_term =
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
    Arg.(value & pos 1 (some path_arg) None & info [] ~doc ~docv:"SPATH")
  in
  let term = Term.(const get $ file_arg $ key_opt_arg) in
  Cmd.v (Cmd.info "get" ~doc ~sdocs ~exits ~man) term, term

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
  Cmd.v (Cmd.info "set" ~doc ~sdocs ~exits ~man)
    Term.(const set $ file_arg $ caret_arg $ sexp)

let locs_cmd =
  let doc = "Show s-expression parse locations" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) outputs s-expression parse locations.";
    `Blocks common_man; ]
  in
  Cmd.v (Cmd.info "locs" ~doc ~sdocs ~exits ~man)
    Term.(const locs $ file_arg)

let cmd =
  let doc = "Process s-expressions" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(mname) edits s-expression files";
    `S Manpage.s_examples;
    `S Manpage.s_bugs;
    `P "This program is distributed with the serialk OCaml library. \
        See $(i,%%PKG_HOMEPAGE%%) for contact information."; ]
  in
  Cmd.group
    (Cmd.info "sexpsk" ~version:"%%VERSION%%" ~doc ~sdocs ~exits ~man)
    ~default:get_term [get_cmd; delete_cmd; locs_cmd; set_cmd;]

let main () = exit (Cmd.eval' cmd)
let () = if !Sys.interactive then () else main ()

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
