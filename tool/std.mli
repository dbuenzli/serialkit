(*---------------------------------------------------------------------------
   Copyright (c) 2023 The cmarkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type fpath = string

module Result : sig
  include module type of Result
  val error_to_failure : ('a, string) result -> 'a

  module Syntax : sig
    val (let*) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
  end
end

module Log : sig
  val err : ('a, Format.formatter, unit, unit) format4 -> 'a
  val warn : ('a, Format.formatter, unit, unit) format4 -> 'a
  val on_error : use:'a -> ('b, string) result -> ('b -> 'a) -> 'a
end

module Os : sig
  val read_file : fpath -> (string, string) result
  val write_file : fpath -> string -> (unit, string) result
  val with_tmp_dir : (fpath -> 'a) -> ('a, string) result
  val with_cwd : fpath -> (unit -> 'a) -> ('a, string) result
end

module Exit : sig
  type code = Cmdliner.Cmd.Exit.code
  val err_file : code
  val err_diff : code
  val exits : Cmdliner.Cmd.Exit.info list
  val exits_with_err_diff : Cmdliner.Cmd.Exit.info list
end

val process_files : (file:fpath -> string -> 'a) -> string list -> Exit.code

module Cli : sig
  open Cmdliner
  val common_man : Manpage.block list
end
