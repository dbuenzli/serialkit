(*---------------------------------------------------------------------------
   Copyright (c) 2023 The cmarkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Std
open Cmdliner

let cmds = [ Cmd_sexp.v; Cmd_toml.v; ]

let serialkit =
  let doc = "Process serialization formats" in
  let exits = Exit.exits_with_err_diff in
  let man = [
    `S Manpage.s_description;
    `P "$(mname) process various serialization formats";
    `Blocks Cli.common_man; ]
  in
  Cmd.group (Cmd.info "serialkit" ~version:"%%VERSION%%" ~doc ~exits ~man) @@
  cmds

let main () = exit (Cmd.eval' serialkit)
let () = if !Sys.interactive then () else main ()
