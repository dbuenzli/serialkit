(*---------------------------------------------------------------------------
   Copyright (c) 2021 The serialkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Std
open Serialkit_toml

let err_file = 1
let err_toml = 2
let err_query = 3

let exec = Filename.basename Sys.executable_name
let log_err fmt =
  Format.fprintf Format.err_formatter ("%s: @[" ^^ fmt ^^ "@]@.") exec

let log_on_error ~exit:code r f = match r with
| Error e -> log_err "%s" e; code | Ok v -> f v

let delete file path =
  failwith "TODO"
(*
  let query = Sexpq.delete_at_path ~must_exist:false path in
  log_on_error ~exit:err_file (File.read file) @@ fun content ->
  log_on_error ~exit:err_sexp (Sexp.seq_of_string' ~file content) @@ fun sexp ->
  log_on_error ~exit:err_query (Sexpq.query' query sexp) @@ fun sexp ->
  Format.printf "@[%a@]" Sexp.pp_seq_layout sexp; 0
*)

let get file path =
  log_on_error ~exit:err_file (Os.read_file file) @@ fun content ->
  log_on_error ~exit:err_toml (Toml.of_string' ~file content) @@ fun toml ->
  match path with
  | None -> Format.printf "@[%a@]" Toml.pp_layout toml; 0
  | Some path ->
      failwith "TODO"
(*
      let query = Sexpq.path path Sexpq.sexp in
      log_on_error ~exit:err_query (Sexpq.query' query sexp) @@ function
      | `A (a, _) | `L ([`A (a, _)], _) -> Format.printf "%s@." a; 0
      | `L _ as l -> Format.printf "@[%a@]@." Sexp.pp l; 0
*)

let locs file =
  failwith "TODO"
(*
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
*)

let set file caret v =
  failwith "TODO"
(*
  log_on_error ~exit:err_file (File.read file) @@ fun content ->
  log_on_error ~exit:err_sexp (Sexp.seq_of_string' ~file content) @@ fun sexp ->
  log_on_error ~exit:err_sexp (Sexp.seq_of_string' v) @@ fun v ->
  let query = Sexpq.splice_at_caret ~must_exist:false caret ~rep:v in
  log_on_error ~exit:err_query (Sexpq.query' query sexp) @@ fun sexp ->
  Format.printf "@[%a@]" Sexp.pp_seq_layout sexp; 0
*)

(* Command line interface *)

open Cmdliner

let file_arg =
  let doc = "$(docv) is the TOML file. Use $(b,-) for stdin." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"FILE")

let path_arg =
  let parse s = match Toml.path_of_string s with
  | Ok _ as v -> v | Error e -> Error (`Msg e)
  in
  Arg.conv ~docv:"TPATH" (parse, Toml.pp_path ())

let caret_arg =
  let parse s = match Toml.caret_of_string s with
  | Ok _ as v -> v | Error e -> Error (`Msg e)
  in
  Arg.conv ~docv:"CARET" (parse, Toml.pp_caret ())

let exits =
  Cmd.Exit.info err_file ~doc:"on file read errors" ::
  Cmd.Exit.info err_toml ~doc:"on TOML parse errors" ::
  Cmd.Exit.info err_query ~doc:"on path query errors" ::
  Cmd.Exit.defaults

let common_man =
  [ `S Manpage.s_bugs;
    `P "This program is distributed with the serialk OCaml library.
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
  Cmd.v (Cmd.info "delete" ~doc ~sdocs ~exits ~man) @@
  Term.(const delete $ file_arg $ path_arg)

let get_cmd =
  let doc = "Extract a TOML path (default)" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) returns the value of a TOML file key.";
    `S Manpage.s_examples;
    `P "TODO";
    `Blocks common_man; ]
  in
  let key_opt_arg =
    let doc = "Extract TOML path $(docv)." in
    Arg.(value & pos 1 (some path_arg) None & info [] ~doc ~docv:"SPATH")
  in
  Cmd.v (Cmd.info "get" ~doc ~sdocs ~exits ~man) @@
  Term.(const get $ file_arg $ key_opt_arg)

let set_cmd =
  let doc = "Edit a TOML file key" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) sets the value of an TOML file key.";
    `S Manpage.s_examples;
    `P "TODO";
    `Blocks common_man; ]
  in
  let caret_arg =
    let doc = "Set caret $(docv)." in
    Arg.(required & pos 1 (some caret_arg) None & info [] ~doc ~docv:"CARET")
  in
  let toml =
    let doc = "$(docv) to insert or substitute" in
    Arg.(required & pos 2 (some string) None & info [] ~doc ~docv:"TOML")
  in
  Cmd.v (Cmd.info "set" ~doc ~sdocs ~exits ~man) @@
  Term.(const set $ file_arg $ caret_arg $ toml)

let locs_cmd =
  let doc = "Show TOML parse locations" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) outputs TOML parse locations.";
    `Blocks common_man; ]
  in
  Cmd.v (Cmd.info "locs" ~doc ~sdocs ~exits ~man) @@
  Term.(const locs $ file_arg)

let cmds = [get_cmd; delete_cmd; locs_cmd; set_cmd;]
let v =
  let doc = "Process TOML" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(mname) edits TOML files";
    `S Manpage.s_examples;
    `S Manpage.s_bugs;
    `P "This program is distributed with the serialk OCaml library.
        See $(i,%%PKG_HOMEPAGE%%) for contact information."; ]
  in
  Cmd.group
    (Cmd.info "toml" ~version:"%%VERSION%%" ~doc ~sdocs ~exits ~man) cmds
