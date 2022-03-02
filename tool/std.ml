(*---------------------------------------------------------------------------
   Copyright (c) 2023 The cmarkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type fpath = string

module Result = struct
  include Result
  let error_to_failure = function Ok v -> v | Error err -> failwith err
  module Syntax = struct
    let ( let* ) = Result.bind
  end
end

module Log = struct
  let exec = Filename.basename Sys.executable_name

  let err fmt =
    Format.fprintf Format.err_formatter ("%s: @[" ^^ fmt ^^ "@]@.") exec

  let warn fmt =
    Format.fprintf Format.err_formatter ("@[" ^^ fmt ^^ "@]@.")

  let on_error ~use r f = match r with
  | Ok v -> f v | Error e -> err "%s" e; use
end

module Os = struct

  (* Emulate B0_std.Os functionality to eschew the dep *)

  let read_file file =
    try
      let ic = if file = "-" then stdin else open_in_bin file in
      let finally () = if file = "-" then () else close_in_noerr ic in
      Fun.protect ~finally @@ fun () -> Ok (In_channel.input_all ic)
    with
    | Sys_error err -> Error err

  let write_file file s =
    try
      let oc = if file = "-" then stdout else open_out_bin file in
      let finally () = if file = "-" then () else close_out_noerr oc in
      Fun.protect ~finally @@ fun () -> Ok (Out_channel.output_string oc s)
    with
    | Sys_error err -> Error err

  let with_tmp_dir f =
    try
      let tmpdir =
        let file = Filename.temp_file "cmarkit" "dir" in
        (Sys.remove file; Sys.mkdir file 0o700; file)
      in
      let finally () = try Sys.rmdir tmpdir with Sys_error _ -> () in
      Fun.protect ~finally @@ fun () -> Ok (f tmpdir)
    with
    | Sys_error err -> Error ("Making temporary dir: " ^ err)

  let with_cwd cwd f =
    try
      let curr = Sys.getcwd () in
      let () = Sys.chdir cwd in
      let finally () = try Sys.chdir curr with Sys_error _ -> () in
      Fun.protect ~finally @@ fun () -> Ok (f ())
    with
    | Sys_error err -> Error ("With cwd: " ^ err)
end

module Exit = struct
  open Cmdliner

  type code = Cmdliner.Cmd.Exit.code
  let err_file = 1
  let err_diff = 2

  let exits =
    Cmd.Exit.info err_file ~doc:"on file read errors." ::
    Cmd.Exit.defaults

  let exits_with_err_diff =
    Cmd.Exit.info err_diff ~doc:"on render differences." :: exits
end

let process_files f files =
  let rec loop = function
  | [] -> 0
  | file :: files ->
      Log.on_error ~use:Exit.err_file (Os.read_file file) @@ fun content ->
      f ~file content; loop files
  in
  loop files

module Cli = struct
  open Cmdliner

  let common_man =
    [ `S Manpage.s_bugs;
      `P "This program is distributed with the $(b,cmarkit) OCaml library. \
          See $(i,https://erratique.ch/software/cmarkit) for contact \
          information.";
      `S Manpage.s_see_also;
      `P "More information about the renderers can be found in the \
          documentation of the $(b,cmarkit) OCaml library. Consult \
          $(b,odig doc cmarkit) or the online documentation." ]
end
