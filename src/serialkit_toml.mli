(*---------------------------------------------------------------------------
   Copyright (c) 2021 The serialkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** TOML support.

    As specified in {{:https://toml.io/en/v1.0.0}TOML v1.0.0}.

    Open this module to use it, this only introduces modules in your scope. *)

open Serialkit_text

(** TOML definitions and codec.

    {b References}
    {ul
    {- Tom Preston-Werner et al,
       {:https://toml.io/en/v1.0.0}{e TOML v1.0.0}}.}} *)
module Toml : sig

  (** {1:toml TOML} *)

  val file_ext : string
  (** [file_ext] is [".toml"]. *)

  val mime_type : string
  (** [mime_type] is ["application/toml"]. *)

  val version : string
  (** [version] is the supported TOML version. *)

  (** Text locations. *)
  module Textloc = Serialkit_text.Textloc

  (** Node metadata. *)
  module Meta = Serialkit_text.Meta

  type 'a node = 'a * Meta.t
  (** The type for abstract syntax tree nodes. The data of type ['a] and its
      metadata. *)

  type tz_offset_s = int
  (** The type for time zone offsets between local and UTC timelines
      in seconds. This is the signed difference in seconds between
      the local timeline and the UTC timeline. *)

  type date = int * int * int
  (** The type dates. A year, a month and a day. *)

  type time = (int * int * int) * float option * tz_offset_s option
  type date_time = date option * time option
  type table = (string node * value) node list

  and value =
  [ `Boolean of bool node
  | `Integer of int64 node
  | `Float of float node
  | `String of string node
  | `Array of value list node
  | `Date_time of date_time node
  | `Table of table node ]

  type t = table node
  (** The type for TOML documents. *)

  (** {1:fmt Formatting} *)

  type 'a fmt = Format.formatter -> 'a -> unit
  (** The type for formatting functions. *)

  val pp : t fmt
  (** [pp] formats TOML. *)

  val pp_layout : t fmt
  (** [pp_layout] is like [pp] but uses layout information. *)

  val pp_value : value fmt
  (** [pp_value] formats TOML values. *)

  (** {1:codec Codec} *)

  (** Decoding errors. *)
  module Error : sig

    type kind
    (** The type for kinds of decoding errors. *)

    val pp_kind : unit -> kind fmt
    (** [pp_error_kind ()] formats an error kind. *)

    type t = kind * Textloc.t
    (** The type for decoding errors. The error kind and its location in
        text. *)

    val pp :
      ?pp_loc:Textloc.t fmt -> ?pp_kind:kind fmt ->
      ?pp_prefix:unit fmt -> unit -> t fmt
    (** [pp ~pp_loc ~pp_error_kind ~pp_prefix ()] formats errors
        using [pp_loc] (defaults to {!pp_loc}), [pp_error_kind]
        (defaults to {!pp_error_kind}) and [pp_prefix] (defaults formats
        ["Error: "]). *)

    val to_string : ?pp:t fmt -> ('a, t) result -> ('a, string) result
    (** [to_string ~pp r] converts an error to a string using [pp]
        (defaults to {!pp}). *)
  end

  val of_string :
    ?layout:bool -> ?locs:bool -> ?file:Textloc.fpath -> string ->
    (t, Error.t) result
  (** [of_string s] parses TOML from [s].

      {ul
      {- [file] is the file path from which [s] is assumed to have been
         read (defaults to {!Textloc.none}).}
      {- If [locs] is [true] (default) locations are stored in nodes of the
         abstract syntax tree in {{!Meta.id}individually identifified} {!Meta.t}
         values. If [false] node meta values are all {!Meta.none} whose text
         location is {!Textloc.none}.}
      {- If [layout] is [false] (default) layout values cannot be relied
         and do not in general represent source layout.}}

      The parser has the following limitations.

      {ul
      {- The ranges of date and time {e fields} are checked but
         dates are not checked for validity. Use your favourite
         date-time library to validate them.}}

      {b Note.} All OCaml strings returned by this function are UTF-8
      encoded. *)

  val of_string' :
    ?pp_error:Error.t fmt -> ?file:Textloc.fpath -> string ->
    (t, string) result
  (** [of_string'] is [of_string] composed with {!error_to_string}. *)

  val to_string : t -> string
  (** [to_string t] is [t] as TOML.

      {b Warning.} Assumes all OCaml strings in [t] are UTF-8 encoded. *)

  (** {1:toml_index TOML indices} *)

  type index =
  | Nth of int (** *)
  | Key of string (** *)
  (** The type for TOML indexing operations.
      {ul
      {- [Nth n], lookup zero-based element [n] in a list. If [n] is
         negative, counts the number of elements from the end of the
         list, i.e. [-1] is the last list element.}
      {- [Key k], lookup binding [k] in an s-expression
         {{!sexp_dict}dictionary.}}} *)

  val pp_key : string fmt
  (** [pp_key] formats a key, this is {!Format.pp_print_string}. *)

  val pp_index : ?pp_key:string fmt -> unit -> index fmt
  (** [pp_index] formats indices. Keys are unbracketed and formatted
      with [pp_key], defaults to {!pp_key}. *)

  (** {1:toml_path TOML paths} *)

  type path = index list
  (** The type for paths, a sequence of indexing operations in {b reverse}
      order. *)

  val path_of_string : string -> (path, string) result
  (** [path_of_string] parses a path from [s] according to the syntax
      {{!sexp_path_caret}given here}. *)

  val pp_path : ?pp_key:string fmt -> unit -> path fmt
  (** [pp_path ?pp_key ()] is a formatter for paths using [pp_key] to
      format keys (defaults to {!pp_key}). *)

  (** {1:carets Carets} *)

  type caret_loc =
  | Before (** The void before the TOML found by the path. *)
  | Over  (** The TOML found by the path. *)
  | After (** The void after the TOML found by the path. *)
  (** The type for caret locations. *)

  type caret = caret_loc * path
  (** The type for carets. A caret location and the path at which it
      applies. *)

  val caret_of_string : string -> (caret, string) result
  (** [caret_of_string s] parses a caret from [s] according to the
      syntax {{!sexp_path_caret}given here}. *)

  val pp_caret : ?pp_key:string fmt -> unit -> caret fmt
  (** [pp_caret ?pp_key ()] is a formatter for carets using [pp_key]
      to format keys (defaults to {!pp_key}). *)
end
