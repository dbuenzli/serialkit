(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** JSON text support.

    Open this module to use it, this only introduces modules in your scope. *)

(** JSON text definitions and codec.

    {b Warning.} The module assumes strings are UTF-8 encoded. *)
module Json : sig

  (** {1:json JSON text} *)

  type loc = Sk_tlex.Tloc.t
  (** The type for text location. *)

  val loc_nil : loc
  (** [loc_nil] is an invalid input location. *)

  type mem = (string * loc) * t
  and t =
  [ `Null of loc
  | `Bool of bool * loc
  | `Float of float * loc
  | `String of string * loc
  | `A of t list * loc
  | `O of mem list * loc ]
  (** The type for generic JSON text representations. *)

  val loc : t -> loc
  (** [loc j] is [j]'s input location. *)

  (** {1:cons Constructors} *)

  val null : t
  (** [null] is [`Null loc_nil]. *)

  val bool : bool -> t
  (** [bool b] is [`Bool (b, loc_nil)]. *)

  val float : float -> t
  (** [float b] is [`Float (f, loc_nil)]. *)

  val string : string -> t
  (** [string s] is [`String (s, loc_nil)]. *)

  val array : t list -> t
  (** [a vs] is [`A (vs, loc_nil)]. *)

  val mem : string -> t -> mem
  (** [mem n v] is [((n, loc_nil), v)]. *)

  val obj : mem list -> t
  (** [obj mems] is [`O (mems, loc_nil)]. *)

  (** {1:access Accessors} *)

  val to_null : t -> (unit, string) result
  (** [to_null j] extracts a null from [j]. If [j] is not a null an
      error with the location formatted according to {!Tloc.pp} is returned. *)

  val to_bool : t -> (bool, string) result
  (** [to_bool j] extracts a bool from [j]. If [j] is not a bool an
      error with the location formatted according to {!Tloc.pp} is returned. *)

  val to_float : t -> (float, string) result
  (** [to_float j] extracts a float from [j]. If [j] is not a float an
      error with the location formatted according to {!Tloc.pp} is returned. *)

  val to_string : t -> (string, string) result
  (** [to_string j] extracts a string from [j]. If [j] is not a string an
      error with the location formatted according to {!Tloc.pp} is returned. *)

  val to_array : t -> (t list, string) result
  (** [to_array j] extracts a array from [j]. If [j] is not a array an
      error with the location formatted according to {!Tloc.pp} is returned. *)

  val to_obj : t -> (mem list, string) result
  (** [to_obj j] extracts a array from [j]. If [j] is not a array an
      error with the location formatted according to {!Tloc.pp} is returned. *)

  val get_null : t -> unit
  (** [get_null s] is like {!to_null} but raises {!Invalid_argument}
      if [s] is not a null. *)

  val get_bool : t -> bool
  (** [get_bool s] is like {!to_bool} but raises {!Invalid_argument}
      if [s] is not a bool. *)

  val get_float : t -> float
  (** [get_float s] is like {!to_float} but raises {!Invalid_argument}
      if [s] is not a float. *)

  val get_string : t -> string
  (** [get_string s] is like {!to_string} but raises {!Invalid_argument}
      if [s] is not a string. *)

  val get_array : t -> t list
  (** [get_array s] is like {!to_array} but raises {!Invalid_argument}
      if [s] is not a array. *)

  val get_obj : t -> mem list
  (** [get_obj s] is like {!to_obj} but raises {!Invalid_argument}
      if [s] is not a array. *)

  (** {1:fmt Formatters} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats JSON text. *)

  (** {1:codec Codec} *)

  val of_string : ?file:Sk_tlex.Tloc.fpath -> string -> (t, string) result
  (** [of_string s] parses JSON text from [s] according to
      {{:https://tools.ietf.org/html/rfc8259}RFC8259} with the following
      limitations:
      {ul
      {- Numbers are parsed with [string_of_float] which is not
         compliant.}
      {- FIXME simply require ocaml with Buffer.add_utf_8
        Unicode escapes are left unparsed (this will not round trip
         with {!to_string}).}} *)

  val to_string : t -> string
  (** [to_string v] is [v] as JSON text, encoded according to
      {{:https://tools.ietf.org/html/rfc8259}RFC8259} *)
end

(** JSON value generation. *)
module Jsong : sig

  (** {1:gen Generation} *)

  type t
  (** The type for generated JSON values. *)

  val null : t
  (** [null] is the generated JSON null value. *)

  val bool : bool -> t
  (** [bool b] is [b] as a generated JSON boolean value. *)

  val int : int -> t
  (** [int i] is [i] as a generated JSON number. *)

  val float : float -> t
  (** [float f] is [f] as a generated JSON number. *)

  val string : string -> t
  (** [str s] is [s] as a generated JSON string value. *)

  type arr
  (** The type for generated JSON arrays. *)

  val arr : arr
  (** [arr] is an empty array. *)

  val arr_end : arr -> t
  (** [arr_end els] is arr a a generated JSON value. *)

  val el : t -> arr -> arr
  (** [el e arr] is array [arr] wit [e] added at the end. *)

  val el_if : bool -> (unit -> t) -> arr -> arr
  (** [el cond v arr] is [el (v ()) arr] if [cond] is [true] and
      [arr] otherwise. *)

  type obj
  (** The type for generated JSON objects. *)

  val obj : obj
  (** [obj] is an empty object. *)

  val obj_end : obj -> t
  (** [obj_end o] is [o] as a generated JSON value. *)

  val mem : string -> t -> obj -> obj
  (** [mem name v o] is [o] with member [name] bound to value [v]
      added. *)

  val mem_if : bool -> string -> (unit -> t) -> obj -> obj
  (** [mem_if cond name v o] is [mem name (v ()) o] if [cond] is [true]
      and [o] otherwise. *)

  (** {1:derived Derived generators} *)

  val strf : ('a, Format.formatter, unit, t) format4 -> 'a
  (** [strf fmt ...] is a JSON string generated value formatted according
      to [fmt]. *)

  val option : ('a -> t) -> 'a option -> t
  (** [option some o] is [o] as a generated JSON value which is
      {!null} if [o] is [None] and [some v] if [o] is [some v]. *)

  val list : ('a -> t) -> 'a list -> t
  (** [list el l] is [l] as a generated JSON array whose elements
      are generated using [el]. *)

  val json : Json.t -> t
  (** [of_json v] is the JSON value [v] as a generated value. *)

  (** {1:output Output} *)

  val buffer_add : Buffer.t -> t -> unit
  (** [buffer_add b g] adds the generated JSON value [g] to [b]. *)

  val to_string : t -> string
  (** [to_string g] is the generated JSON value [g] as a string. *)
end

(** JSON value queries. *)
module Jsonq : sig

  (** {1:query Queries} *)

  type 'a t
  (** The type for a query on a JSON value returning values of type ['a]. *)

  val null : unit t
  (** [null] queries a null JSON value. *)

  val nullable : 'a t -> 'a option t
  (** [nullable q] queries either a null JSON value or with [q]. *)

  val bool : bool t
  (** [bool] queries a boolean JSON value. *)

  val int : int t
  (** [int] queries a float JSON value and {!truncate}s it. *)

  val float : float t
  (** [float] queries a float JSON value. *)

  val string : string t
  (** [string] queries a string JSON value. *)

  val array : 'a t -> 'a list t
  (** [array q] queries the elements of a JSON array with [q]. *)

  val mem : string -> 'a t -> ('a -> 'b) t -> 'b t
  (** [mem name q o] queries a JSON object [o]'s member named
      [name] with [q]. *)

  val mem_opt : string -> 'a t -> ('a option -> 'b) t -> 'b t
  (** [mem_opt name q] queries a JSON object [o]'s optional member named
      [name] with [q]. *)

  val obj : 'a -> 'a t
  (** [obj v] queries an object and returns [v]. *)

  val json : Json.t t
  (** [json] queries any JSON value. *)

  val get : 'a -> 'a
  (** [get] is the identity function *)

  val sel : string -> 'a t -> 'a t
  (** [sel name q] is [obj get |> mem name q] *)

  val query : 'a t -> Json.t -> ('a, string) result
  (** [query q j] queries a JSON value [j] with [q]. *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers

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
