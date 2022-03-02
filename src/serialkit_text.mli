(*---------------------------------------------------------------------------
   Copyright (c) 2019 The serialkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** UTF-8 text based formats tools.

    Open this module to use it defines only module in your scope. *)

(**/**)
module Dict : sig
  type 'a key
  val key : unit -> 'a key
  type t
  val empty : t
  val mem : 'a key -> t -> bool
  val add : 'a key -> 'a -> t -> t
  val tag : unit key -> t -> t
  val remove : 'a key -> t -> t
  val find : 'a key -> t -> 'a option
end
(**/**)

(** Text locations.

    A text location identifies a text span in a given UTF-8 encoded file
    by an inclusive range of absolute {{!Textloc.type-byte_pos}byte} positions
    and the {{!Textloc.type-line_pos}line positions} on which those occur. *)
module Textloc : sig

  (** {1:fpath File paths} *)

  type fpath = string
  (** The type for file paths. *)

  val file_none : fpath
  (** [file_none] is ["-"]. A file path to use when there is none. *)

  (** {1:pos Positions} *)

  (** {2:byte_pos Byte positions} *)

  type byte_pos = int
  (** The type for zero-based, absolute, byte positions in text. If
      the text has [n] bytes, [0] is the first position and [n-1] is
      the last position. *)

  val byte_pos_none : byte_pos
  (** [byte_pos_none] is [-1]. A position to use when there is none. *)

  (** {2:lines Lines} *)

  type line_num = int
  (** The type for one-based, line numbers in the text. Lines
      increment after a {e newline} which is either a line feed ['\n']
      (U+000A), a carriage return ['\r'] (U+000D) or a carriage return
      and a line feed ["\r\n"] (<U+000D,U+000A>). *)

  val line_num_none : line_num
  (** [line_num_none] is [-1]. A line number to use when there is none. *)

  (** {2:line_pos Line positions} *)

  type line_pos = line_num * byte_pos
  (** The type for line positions. This identifies a line by its line
      number and the absolute byte position following its newline
      (or the start of text for the first line). That byte position:
      {ul
      {- Indexes the first byte of text of the line if the line is non-empty.}
      {- Indexes the first byte of the next newline if the line is empty.}
      {- Is out of bounds and equal to the text's length for a last empty
         line (this includes when the text is empty).}} *)

  val line_pos_first : line_pos
  (** [line_pos_first] is [1, 0]. Note that this is the only line position
      of the empty text. *)

  val line_pos_none : line_pos
  (** [line_pos_none] is [(line_none, pos_none)]. *)

  (** {1:tloc Text locations} *)

  type t
  (** The type for text locations. A text location identifies a text
      span in an UTF-8 encoded file by an inclusive range of absolute
      {{!type-byte_pos}byte positions} and the {{!type-line_pos}line positions}
      on which they occur.

      If the first byte equals the last byte the range contains
      exactly that byte. If the first byte is greater than the last
      byte this represents an insertion point before the first byte. In
      this case information about the last position should be ignored:
      it can contain anything. *)

  val none : t
  (** [none] is a position to use when there is none. *)

  val v :
    file:fpath -> first_byte:byte_pos -> last_byte:byte_pos ->
    first_line:line_pos -> last_line:line_pos -> t
  (** [v ~file ~first_byte ~last_byte ~first_line ~last_line] is a text
      location with the given arguments, see corresponding accessors for
      the semantics. If you don't have a file use {!file_none}. *)

  val file : t -> fpath
  (** [file l] is [l]'s file. *)

  val first_byte : t -> byte_pos
  (** [first_byte l] is [l]'s first byte. Irrelevant if {!is_none} is
      [true]. *)

  val last_byte : t -> byte_pos
  (** [last_byte l] is [l]'s last byte. Irrelevant if {!is_none} or {!is_empty}
      is [true]. *)

  val first_line : t -> line_pos
  (** [first_line l] is the line position on which [first_byte l] lies.
      Irrelevant if {!is_none} is [true].*)

  val last_line : t -> line_pos
  (** [last_line l] is the line position on which [last_byte l] lies.
      Irrelevant if {!is_none} or {!is_empty} is [true].*)

  (** {2:preds Predicates and comparisons} *)

  val is_none : t -> bool
  (** [is_none t] is [true] iff [first_byte < 0]. *)

  val is_empty : t -> bool
  (** [is_empty t] is [true] iff [first_byte t > last_byte t]. *)

  val equal : t -> t -> bool
  (** [equal t0 t1] is [true] iff [t0] and [t1] are equal. This checks
      that {!file}, {!first_byte} and {!last_byte} are equal. Line information
      is ignored. *)

  val compare : t -> t -> int
  (** [compare t0 t1] orders [t0] and [t1]. The order is compatible
      with {!equal}. Comparison starts with {!file}, follows with {!first_byte}
      and ends, if needed, with {!last_byte}. Line information is ignored. *)

  (** {2:shrink_and_stretch Shrink and stretch} *)

  val set_first : t -> first_byte:byte_pos -> first_line:line_pos -> t
  (** [set_first l ~first_byte ~first_line] sets the the first position of
      [l] to given values. *)

  val set_last : t -> last_byte:byte_pos -> last_line:line_pos -> t
  (** [set_last l ~last_byte ~last_line] sets the last position of [l]
      to given values. *)

  val to_first : t -> t
  (** [to_first l] has both first and last positions set to [l]'s first
      position. The range spans {!first_byte}. See also {!before}. *)

  val to_last : t -> t
  (** [to_last l] has both first and last positions set to [l]'s last
      position. The range spans {!last_byte}. See also {!after}. *)

  val before : t -> t
  (** [before t] is the {{!is_empty}empty} text location starting at
      {!first_byte}. *)

  val after : t -> t
  (** [after t] is the empty {{!is_empty}empty} location starting at
      [last_byte t + 1]; note that at the end of input this may be an
      invalid byte {e index}. The {!first_line} and {!last_line} of the
      result is [last_line t]. *)

  val span : t -> t -> t
  (** [span l0 l1] is the span from the smallest byte position of [l0] and
      [l1] to the largest byte position of [l0] and [l1]. The file path is
      taken from the greatest byte position. *)

  val reloc : first:t -> last:t -> t
  (** [reloc ~first ~last] uses the first position of [first], the
      last position of [last] and the file of [last]. *)

  (** {2:fmt Formatting} *)

  val pp_ocaml : Format.formatter -> t -> unit
  (** [pp_ocaml] formats text locations like the OCaml compiler. *)

  val pp_gnu : Format.formatter -> t -> unit
  (** [pp_gnu] formats text locations according to the
      {{:https://www.gnu.org/prep/standards/standards.html#Errors}GNU
      convention}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] is {!pp_gnu}. *)

  val pp_dump : Format.formatter -> t -> unit
  (** [pp_dump] formats raw data for debugging. *)
end

(** Text node metadata.

    Holds text locations and custom, client-defined metadata. *)
module Meta : sig

  type id = int
  (** The type for non-negative metadata identifiers. *)

  type t
  (** The type for abstract syntax tree node metadata. *)

  val none : t
  (** [none] is metadata for when there is none, its {!textloc} is
      {!Textloc.none}. *)

  val make : ?textloc:Textloc.t -> unit -> t
  (** [make textloc] is metadata with text location [textloc] (defaults
      to {!Textloc.none}) and a fresh identifier (see {!val-id}). *)

  val id : t -> id
  (** [id m] is an identifier for the metadata. Depending on how you
      process the abstract syntax tree this may become non-unique but
      the metadata values in an abstract syntax tree returned by
      {!Doc.of_string} with [locs:true] have distinct identifiers. *)

  val textloc : t -> Textloc.t
  (** [textloc m] is the source location of the syntactic construct [m]
      is attached to. *)

  val with_textloc : keep_id:bool -> t -> Textloc.t -> t
  (** [with_textloc ~keep_id m textloc] is metadata [m] with text location
      [textloc] and a fresh id, unless [keep_id] is [true]. *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal m0 m1] is [true] if [m0] and [m1] have the same {!val-id}.
      Note that they may have different {{!custom}metadata.} *)

  val compare : t -> t -> int
  (** [compare m0 m1] is a total order on metadata {!val-id}s compatible with
      {!equal}. *)

  val is_none : t -> bool
  (** [is_none m] is [equal none m]. *)

  (** {1:custom Custom metadata}

      {b Warning.} Operating on custom metadata never changes
      {!val-id}. It is possible for two meta values to have the same
      id and different metadata. *)

  type 'a key
  (** The type for custom metadata keys. *)

  val key : unit -> 'a key
  (** [key ()] is a new metadata key. *)

  val mem : 'a key -> t -> bool
  (** [mem k m] is [true] iff [k] is bound in [m]. *)

  val add : 'a key -> 'a -> t -> t
  (** [add k v m] is [m] with key [k] bound to [v]. *)

  val tag : unit key -> t -> t
  (** [tag k m] is [add k () m]. *)

  val remove : 'a key -> t -> t
  (** [remove k m] is [m] with key [k] unbound in [v]. *)

  val find : 'a key -> t -> 'a option
  (** [find k m] the value of [k] in [m], if any. *)
end

(** UTF-8 text decoder.

    A decoder inputs {e valid} UTF-8 text, maintains {{!Textloc}text locations}
    according to advances in the input and has a {{!Textdec.lex}lexeme buffer}
    for lexing. *)
module Textdec : sig

  (** {1:dec Decoder} *)

  type t
  (** The type for UTF-8 text decoders. *)

  val from_string : ?file:Textloc.fpath -> string -> t
  (** [from_string ~file s] decodes [s] using [file] (defaults to
      {!Tloc.no_file}) for text location. *)

  (** {1:loc Locations} *)

  val file : t -> Textloc.fpath
  (** [file d] is the input file. *)

  val pos : t -> Textloc.byte_pos
  (** [pos d] is the current decoding byte position. *)

  val line : t -> Textloc.line_pos
  (** [line d] is the current decoding line position. Lines increment as
      described {{!Tloc.line}here}. *)

  val loc :
    t -> first_byte:Textloc.byte_pos -> last_byte:Textloc.byte_pos ->
    first_line:Textloc.line_pos -> last_line:Textloc.line_pos -> Textloc.t
  (** [loc d ~first_byte ~last_bytex ~first_line ~last_line] is
      {!Tloc.v} using [file d] for the file. *)

  val loc_to_here :
    t -> first_byte:Textloc.byte_pos -> first_line:Textloc.line_pos -> Textloc.t
  (** [loc_to_here d ~first_byte ~first_line] is a location that starts at
      [~first_byte] and [~first_line] and ends at the current decoding
      position as determined by {!pos} and {!line}. *)

  val loc_here : t -> Textloc.t
  (** [loc_here d] is like {!loc_to_here} with the start position
      at the current decoding position as determined by
      {!pos} and {!line}. *)

  (** {1:err Errors} *)

  exception Err of Textloc.t * string
  (** The exception for errors. A location and an english error message *)

  val err : Textloc.t -> string -> 'b
  (** [err loc msg] raises [Err (loc, msg)] with no trace. *)

  val err_to_here :
    t -> first_byte:Textloc.byte_pos -> first_line:Textloc.line_pos ->
    ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** [err_to_here d ~first_byte ~first_line fmt ...] is
      [err d (loc_to_here d ~first_byte ~first_line) fmt ...] *)

  val err_here : t -> ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** [err_here d] is [err d (loc_here d) fmt ...]. *)

  (** {2:err_msg Error message helpers} *)

  val err_suggest : ?dist:int -> string list -> string -> string list
  (** [err_suggest ~dist candidates s] are the elements of
      [candidates] whose edit distance is the smallest to [s] and at
      most at a distance of [dist] of [s] (defaults to [2]). If
      multiple results are returned the order of [candidates] is
      preserved. *)

  type 'a fmt = Format.formatter -> 'a -> unit
  (** The type for formatters. *)

  val pp_and_enum : ?empty:unit fmt -> 'a fmt -> 'a list fmt
  (** [and_enum ~empty pp_v ppf l] formats [l] according to its length.
      {ul
      {- [0], formats {!empty} (defaults to {!nop}).}
      {- [1], formats the element with [pp_v].}
      {- [2], formats ["%a and %a"] with the list elements}
      {- [n], formats ["%a, ... and %a"] with the list elements}} *)

  val pp_or_enum : ?empty:unit fmt -> 'a fmt -> 'a list fmt
  (** [or_enum] is like {!and_enum} but uses "or" instead of "and". *)

  val pp_did_you_mean : 'a fmt -> 'a list fmt
  (** [did_you_mean pp_v] formats ["Did you mean %a ?"] with {!or_enum}
      if the list is non-empty and {!nop} otherwise. *)

  val pp_must_be : 'a fmt -> 'a list fmt
  (** [must_be pp_v] formats ["Must be %a."] with {!or_enum} if the list
      is non-empty and {!nop} otherwise. *)

  val pp_unknown : kind:unit fmt -> 'a fmt -> 'a fmt
  (** [unknown ~kind pp_v] formats ["Unknown %a %a." kind () pp_v]. *)

  val pp_unknown' :
    kind:unit fmt -> 'a fmt -> hint:('a fmt -> 'a list fmt) ->
    ('a * 'a list) fmt
  (** [unknown ~kind pp_v ~hint (v, hints)] formats {!unknown} followed
      by a space and [hint pp_v hints] if [hints] is non-empty. *)

  (** {1:dec Decoding} *)

  val eoi : t -> bool
  (** [eoi d] is [true] iff the decoder is at the end of input. *)

  val byte : t -> int
  (** [byte d] is the byte at current position or [0xFFFF] if
      [eoi d] is [true]. *)

  val accept_uchar : t -> unit
  (** [accept_uchar d] accepts an UTF-8 encoded character starting at
      the current position and moves to the byte location after it. Raises
      {!Err} in case of UTF-8 decoding error. *)

  val accept_byte : t -> unit
  (** [accept_byte d] accepts the byte at the current position and
      moves to the byte location after it. {b Warning.} Faster than
      {!accept_uchar} but the client needs to make sure it's not
      accepting invalid UTF-8 data, i.e. that [byte d] is an US-ASCII
      encoded character (i.e. [<= 0x7F]). *)

  (** {1:lex Lexeme buffer} *)

  val lex_clear : t -> unit
  (** [lex_clear d] sets the lexeme to the empty string. *)

  val lex_pop : t -> string
  (** [lex_pop d] is the lexeme and {!lex_clear}s it. *)

  val lex_add_byte : t -> int -> unit
  (** [lex_add_byte d b] adds byte [b] to the lexen. *)

  val lex_add_bytes : t -> string -> unit
  (** [lex_add_byte d s] adds bytes [s] to the lexen. *)

  val lex_add_char : t -> char -> unit
  (** [lex_add_char d c] adds character [c] to the lexen. *)

  val lex_add_uchar : t -> Uchar.t -> unit
  (** [lex_add_uchar t u] adds the UTF-8 encoding of character [u]
      to the lexen. *)

  val lex_accept_uchar : t -> unit
  (** [lex_accept_uchar d] is like {!accept_uchar} but also adds the
      UTF-8 byte sequence to the lexeme. *)

  val lex_accept_byte : t -> unit
  (** [lex_accept_byte d] is like {!accept_byte} but also adds the
      byte to the lexeme. {b Warning.} {!accept_byte}'s warning
      applies. *)

  (** {1:text String substitutions and insertions}

      Strictly speaking this doesn't belong here but here you go. *)

  val string_subrange : ?first:int -> ?last:int -> string -> string
  (** [string_subrange ~first ~last s] are the consecutive bytes of [s]
      whose indices exist in the range \[[first];[last]\].

      [first] defaults to [0] and last to [String.length s - 1].

      Note that both [first] and [last] can be any integer. If
      [first > last] the interval is empty and the empty string is
      returned. *)

  val string_replace : start:int -> stop:int -> rep:string -> string -> string
  (** [string_replace ~start ~stop ~rep s] replaces the index range
      \[[start];stop-1\] of [s] with [rep] as follows. If [start = stop]
      the [rep] is inserted before [start]. [start] and [stop] must be
      in range \[[0];[String.length s]\] and [start <= stop] or
      [Invalid_argument] is raised. *)
end
