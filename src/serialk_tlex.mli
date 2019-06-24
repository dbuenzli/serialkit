(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** UTF-8 text lexing tools.

    Open this module to use it defines only module in your scope. *)

(** Text locations. *)
module Tloc : sig

  (** {1:tloc Text locations} *)

  type fpath = string
  (** The type for file paths. *)

  val no_file : fpath
  (** [no_file] is [Fpath.t "-"], a path used when no file is specified. *)

  type pos = int
  (** The type for zero-based, absolute, byte positions in text. *)

  type line = int
  (** The type for one-based, line numbers in the text. Usually lines
      increment after a line feed ['\n'] (U+000A), a carriage return
      ['\r'] (U+000D) or a carriage return and a line feed ["\r\n"]
      (<U+000D,U+000A>). *)

  type t
  (** The type for text locations. A text location is a range of
      byte positions and the lines on which they occur in the UTF-8 encoded
      text of a particular file. *)

  val v :
    file:fpath -> byte_s:pos -> byte_e:pos -> line_s:pos * line ->
    line_e:pos * line -> t
  (** [v ~file ~byte_s ~byte_e ~line_s ~line_e] is a contructor for
      text locations. See corresponding accessors for the semantics.
      If you don't have a file use {!no_file}. *)

  val file : t -> fpath
  (** [file l] is [l]'s file. *)

  val byte_s : t -> pos
  (** [byte_s l] is [l]'s start position. *)

  val byte_e : t -> pos
  (** [byte_e l] is [l]'s end position. *)

  val line_s : t -> pos * line
  (** [line_s l] is the line's position on which [byte_s l] lies and
      the one-based line number. *)

  val line_e : t -> pos * line
  (** [line_e l] is the line's position on which [byte_e l] lies and
      the one-based line number. *)

  val nil : t
  (** [loc_nil] is an invalid location. *)

  val merge : t -> t -> t
  (** [merge l0 l1] merges the location [l0] and [l1] to the smallest
      location that spans both location. The file path is [l0]'s. *)

  val to_start : t -> t
  (** [to_start l] has both start and end positions at [l]'s start. *)

  val to_end : t -> t
  (** [to_end l] has both start and end positions at [l]'s end. *)

  val with_start : t -> t -> t
  (** [with_start s l] is [l] with the start position of [s]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp_loc] formats locations using the
      {{:https://www.gnu.org/prep/standards/standards.html#Errors}GNU
      convention}. *)
end

(** Text decoder.

    A text decoder inputs UTF-8 data and checks its validity.  It
    updates locations according to advances in the input and has a
    token buffer used for lexing. *)
module Tdec : sig

  (** {1:dec Decoder} *)

  type t
  (** The type for UTF-8 text decoders. *)

  val create : ?file:Tloc.fpath -> string -> t
  (** [create ~file input] decodes [input] using [file]
      (defaults to {!Tloc.no_file}) for text location. *)

  (** {1:loc Locations} *)

  val file : t -> Tloc.fpath
  (** [file d] is the input file. *)

  val pos : t -> Tloc.pos
  (** [pos d] is the current decoding position. *)

  val line : t -> Tloc.pos * Tloc.line
  (** [line d] is the current line position. Lines increment as
      described {{!Tloc.line}here}. *)

  val loc_to_here :
    t -> byte_s:Tloc.pos -> line_s:Tloc.pos * Tloc.line -> Tloc.t
  (** [loc_to_here d ~byte_s ~line_s] is a location that starts at
      [~byte_s] and [~line_s] and ends at the current decoding
      position. *)

  val loc_here : t -> Tloc.t
  (** [loc_here d] is like {!loc_to_here} with the start position
      at the current decoding position. *)

  val loc :
    t -> byte_s:Tloc.pos -> byte_e:Tloc.pos -> line_s:Tloc.pos * Tloc.line ->
    line_e:Tloc.pos * Tloc.line -> Tloc.t
  (** [loc d ~byte_s ~byte_e ~line_s ~line_e] is a location with
      the correponding position range. *)

  (** {1:err Errors} *)

  exception Err of Tloc.t * string
  (** The exception for errors. A location and an error message *)

  val err : Tloc.t -> string -> 'b
  (** [err loc msg] raises [Err (loc, msg)] with no trace. *)

  val err_to_here :
    t -> byte_s:Tloc.pos -> line_s:Tloc.pos * Tloc.line ->
    ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** [err_to_here d ~byte_s ~line_s fmt ...] raises [Err] with no
      trace. The location spans from the given start position to the
      current decoding position and the message is formatted according
      to [fmt]. *)

  val err_here : t -> ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** [err_here d] is like {!err_to_here} with the start position
      at the current decoding position. *)

  (** {2:err_msg Error message helpers} *)

  val err_suggest : ?dist:int -> string list -> string -> string list
  (** [err_suggest ~dist candidates s] are the elements of [candidates]
      whose {{!edit_distance}edit distance} is the smallest to [s] and
      at most at a distance of [dist] of [s] (defaults to [2]). If
      multiple results are returned the order of [candidates] is
      preserved. *)

  val err_did_you_mean :
    ?pre:(Format.formatter -> unit -> unit) ->
    ?post:(Format.formatter -> unit -> unit) ->
    kind:string -> (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a * 'a list -> unit
  (** [did_you_mean ~pre kind ~post pp_v] formats a faulty value [v] of
      kind [kind] and a list of [hints] that [v] could have been
      mistaken for.

      [pre] defaults to [unit "Unknown"], [post] to {!nop} they surround
      the faulty value before the "did you mean" part as follows ["%a %s
      %a%a." pre () kind pp_v v post ()]. If [hints] is empty no "did
      you mean" part is printed. *)

  (** {1:dec Decoding} *)

  val eoi : t -> bool
  (** [eoi d] is [true] iff the decoder is at the end of input. *)

  val byte : t -> int
  (** [byte d] is the byte at current position or [0xFFFF] if
      [eoi d] is [true]. *)

  val accept_uchar : t -> unit
  (** [accept_uchar d] accepts an UTF-8 encoded character starting at
      the current position and moves to the byte after it. Raises
      {!Err} in case of UTF-8 decoding error. *)

  val accept_byte : t -> unit
  (** [accept_byte d] accepts the byte at the current position and
      moves to the next byte. {b Warning.} Faster than {!accept_uchar}
      but the client needs to make sure it's not accepting invalid
      UTF-8 data, i.e. that [byte d] is an US-ASCII encoded character
      (i.e. [<= 0x7F]). *)

  (** {1:tok Token buffer} *)

  val tok_reset : t -> unit
  (** [tok_reset d] resets the token. *)

  val tok_pop : t -> string
  (** [tok_pop d] returns the token and {!tok_reset}s it. *)

    val tok_accept_uchar : t -> unit
  (** [tok_accept_uchar d] is like {!accept_uchar} but also
      adds the UTF-8 byte sequence to the token. *)

  val tok_accept_byte : t -> unit
  (** [tok_accept_byte d] is like {!accept_byte} but also
      adds the byte to the token. {b Warning.} {!accept_byte}'s
      warning applies. *)

  val tok_add_byte : t -> int -> unit
  (** [tok_add_byte d b] adds byte [b] to the token. *)

  val tok_add_bytes : t -> string -> unit
  (** [tok_add_byte d s] adds bytes [s] to the token. *)

  val tok_add_char : t -> char -> unit
  (** [tok_add_char d b] adds character [b] to the token. *)

  val tok_add_uchar : t -> Uchar.t -> unit
  (** [tok_add_uchar t u] adds the UTF-8 encoding of character [u]
      to the token. *)
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
