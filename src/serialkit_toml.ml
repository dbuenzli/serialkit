(*---------------------------------------------------------------------------
   Copyright (c) 2021 The serialkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Serialkit_text

let max_month_day =           (* max day number in a given year's month. *)
  let is_leap_year y = (y mod 4 = 0) && (y mod 100 <> 0 || y mod 400 = 0) in
  let mlen = [|31; 28 (* or not *); 31; 30; 31; 30; 31; 31; 30; 31; 30; 31|] in
  fun y m -> if (m = 2 && is_leap_year y) then 29 else mlen.(m - 1)

let is_date_valid (y, m, d) =
  0 <= y && y <= 9999 &&
  1 <= m && m <= 12 &&
  1 <= d && d <= max_month_day y m

let is_time_valid (hh, mm, ss) =
  0 <= hh && hh <= 23 &&
  0 <= mm && mm <= 59 &&
  0 <= ss && ss <= 60

module Toml = struct
  let file_ext = ".toml"
  let mime_type = "application/toml"
  let version = "1.0.0"

  module Textloc = Serialkit_text.Textloc
  module Meta = Serialkit_text.Meta

  type 'a node = 'a * Meta.t

  type tz_offset_s = int
  type date = int * int * int

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

  (* Formatting *)

  type 'a fmt = Format.formatter -> 'a -> unit
  let pf = Format.fprintf
  let pp_char = Format.pp_print_char
  let pp_str = Format.pp_print_string
  let pp_sp = Format.pp_print_space
  let pp_date ppf (y, m, d) = pf ppf "%04d-%02d-%02d" y m d
  let pp_time ppf ((hh, mm, ss), frac, tz) = failwith "TODO"
  let pp_date_time ppf (date, time) = match date, time with
  | None, None -> assert false
  | Some d, None -> pp_date ppf d
  | None, Some t -> pp_time ppf t
  | Some d, Some t -> pp_date ppf d; pp_char ppf ' '; pp_time ppf t

  let rec pp_value : value fmt = fun ppf -> function
  | `Boolean (b, _) -> Format.pp_print_bool ppf b
  | `Integer (i, _) -> pf ppf "%Ld" i
  | `Float (f, _) -> pf ppf "%.16g" f
  | `String (s, _) -> pf ppf "%S" s
  | `Array (l, _) ->
      let pp_sep ppf () = pp_char ppf ','; pp_sp ppf () in
      Format.pp_open_box ppf 2;
      Format.pp_print_list ~pp_sep pp_value ppf l;
      Format.pp_close_box ppf ()
  | `Date_time (dt, _) -> pp_date_time ppf dt
  | `Table (t, _) ->
      let pp_binding ppf (((k, _), v), _) =
        pf ppf "@[<h>%s =@ %a@]" k pp_value v
      in
      Format.pp_print_list ~pp_sep:Format.pp_print_cut pp_binding ppf t

  let pp ppf v = pp_value ppf (`Table v)
  let pp_layout = (* TODO *) pp

  (* Decode errors *)

  module Error = struct
    type expected =
    | Basic_char
    | Bin_digit
    | Char of char
    | Comment
    | Comment_char
    | Date_time
    | Dec_digit
    | Eoi
    | Equal_char
    | False
    | Float
    | Float_exp
    | Float_frac
    | Hex_digit
    | Inf
    | Integer
    | Key
    | Keyval
    | Lf
    | Literal_char
    | Nan
    | Newline
    | Oct_digit
    | Table
    | Tz_offset
    | True
    | Value

    let pp_expected ppf = function
    | Basic_char -> pp_str ppf "a basic string character"
    | Bin_digit -> pp_str ppf "a binary digit"
    | Char c -> pp_str ppf "a '%C' character"
    | Comment -> pp_str ppf "a comment"
    | Comment_char -> pp_str ppf "a comment character"
    | Date_time -> pp_str ppf "a date-time"
    | Dec_digit -> pp_str ppf "a decimal digit"
    | Eoi -> pp_str ppf "end of input"
    | Equal_char -> pp_str ppf "an equal character ('=')"
    | Hex_digit -> pp_str ppf "an hexadecimal digit"
    | False -> pp_str ppf "'true'"
    | Float -> pp_str ppf "float"
    | Float_exp -> pp_str ppf "float exponent"
    | Float_frac -> pp_str ppf "float fractional part"
    | Inf -> pp_str ppf "'inf'"
    | Integer -> pp_str ppf "integer"
    | Key -> pp_str ppf "a key"
    | Keyval -> pp_str ppf "a key/value pair"
    | Lf -> pp_str ppf "a line feed (\\n)"
    | Literal_char -> pp_str ppf "a string literal character"
    | Nan -> pp_str ppf "'nan'"
    | Newline -> pp_str ppf "a newline ('\\n')"
    | Oct_digit -> pp_str ppf "an octal digit"
    | Table -> pp_str ppf "a table"
    | Tz_offset -> pp_str ppf "a timezone offset"
    | True -> pp_str ppf "'true'"
    | Value -> pp_str ppf "a value"

    type unclosed =
    | Basic_string | Basic_multiline_string | Literal_string
    | Literal_multiline_string

    let pp_unclosed ppf = function
    | Basic_string -> pp_str ppf "Unclosed basic string"
    | Basic_multiline_string -> pp_str ppf "Unclosed multiline basic string"
    | Literal_string -> pp_str ppf "Unclosed literal string"
    | Literal_multiline_string -> pp_str ppf "Unclosed multiline literal string"

    type escape =
    | Not_escape_char of char
    | Not_uchar_value of int
    | Invalid_uchar_escape

    let pp_escape ppf = function
    | Not_escape_char c -> pf ppf "%C is not a valid escape character" c
    | Not_uchar_value i -> pf ppf "%04X is not a Unicode character" i
    | Invalid_uchar_escape -> pf ppf "Invalid Unicode character escape"

    type invalid =
    | Leading_zeros
    | Int64_overflow
    | Date of date
    | Hour of int
    | Minute of int
    | Second of int

    let pp_invalid ppf = function
    | Leading_zeros -> pf ppf "Leading zeros are not allowed"
    | Int64_overflow -> pf ppf "Integer overflows 64-bit integers"
    | Date (y, m, d) -> pf ppf "%04d-%02d-%02d is not a valid date" y m d
    | Hour i -> pf ppf "%02d is not a valid hour (00-23)" i
    | Minute i -> pf ppf "%02d is not a valid minute (00-59)" i
    | Second i -> pf ppf "%02d is not a valid second (00-60)" i

    type kind =
    | Escape of escape
    | Expected of expected list
    | Invalid of invalid
    | Unclosed of unclosed

    let pp_kind () ppf = function
    | Escape esc -> pp_escape ppf esc
    | Expected exps -> Textdec.pp_or_enum pp_expected ppf exps
    | Invalid inv -> pp_invalid ppf inv
    | Unclosed uncl -> pp_unclosed ppf uncl

    type t = kind * Textloc.t
    let pp_prefix ppf () = Format.pp_print_string ppf "Error: "
    let pp
        ?(pp_loc = Textloc.pp) ?(pp_kind = pp_kind ())
        ?(pp_prefix = pp_prefix) () ppf (k, l)
      =
      pf ppf "@[<v>%a:@,%a%a@]" pp_loc l pp_prefix () pp_kind k

    let to_string ?(pp = pp ()) = function
    | Ok _ as v -> v | Error e -> Error (Format.asprintf "%a" pp e)
  end

  (* Decoder *)

  type decoder =
    { file : Textloc.fpath;
      s : string;
      nolocs : bool;
      nolayout : bool;
      max_pos : int; (* String.length i - 1 *)
      mutable pos : int; (* current byte position. *)
      mutable line_pos : Textloc.line_pos; (* line position of [pos] *)
      buf : Buffer.t; }

  let decoder ?(layout = false) ?(locs = true) ?(file = Textloc.file_none) s =
    let nolocs = not locs and nolayout = not layout in

    { file; s; nolocs; nolayout; max_pos = String.length s - 1;
      pos = 0; line_pos = Textloc.line_pos_first;
      buf = Buffer.create 255 }

  let tloc_line_span d ~first ~last =
    Textloc.v ~file:d.file ~first_byte:first ~last_byte:last
      ~first_line:d.line_pos ~last_line:d.line_pos

  (* Decoding errors *)

  exception Err of Error.t

  let dec_error_escape d ~first ~last esc =
    let tloc = tloc_line_span d ~first ~last in
    raise (Err (Escape esc, tloc))

  let dec_error_expected d ~first ?(last = first) exps =
    let tloc = tloc_line_span d ~first ~last in
    raise (Err (Expected exps, tloc))

  let dec_error_invalid d ~first ?(last = first) inv =
    let tloc = tloc_line_span d ~first ~last in
    raise (Err (Invalid inv, tloc))

  let dec_error_unclosed d ~first uncl =
    let tloc = tloc_line_span d ~first ~last:d.max_pos in
    raise (Err (Unclosed uncl, tloc))

  let dec_error_eoi d ~first exps =
    let tloc = tloc_line_span d ~first ~last:d.max_pos in
    raise (Err (Expected exps, tloc))

  (* AST *)

  (* The type structure below closely mirrors the TOML ABNF. *)

  type comment = Textloc.t
  (* if non empty includes both the # and the newline *)
  type ws = Textloc.t (* may include newlines if there's no comment or data. *)
  type ws_comment_nl = ws * comment (* either has the newline, or none if eoi *)
  type basic_string = string
  type literal_string = string

  type simple_key = [`Bare | `Quoted_basic | `Quoted_literal ] * string
  type key = (ws * simple_key * ws) list

  type keyval = key * ws * tval
  and tval =
  | String
  | Boolean of bool
  | Array of ws_comment_nl * tval
  | Inline_table of ws * (keyval * ws) list
  | Date_time of date_time
  | Float of float
  | Integer of int64

  type expression =
  | Whitespace of ws_comment_nl
  | Keyval' of keyval * ws_comment_nl
  | Table' of ws * [`Std | `Array] * key * ws_comment_nl

  type toml = expression list

  (* Decoding functions *)

  let dec_ws d = (* ws *)
    let rec loop s max i =
      if i > max then i else match s.[i] with
      | ' ' | '\t' -> loop s max (i + 1)
      | _ -> i
    in
    let next = loop d.s d.max_pos d.pos in
    let ws = tloc_line_span d ~first:d.pos ~last:(next - 1) in
    d.pos <- next;
    ws

  let dec_newline d ws = (* newline, first newline checked, added to ws *)
    let next =
      if d.s.[d.pos] = '\n' then d.pos + 1 else (* '\r' *)
      let next = d.pos + 1 in
      if next > d.max_pos || d.s.[next] <> '\n'
      then dec_error_expected d ~first:next [Lf]
      else next + 1
    in
    let line_pos = fst d.line_pos + 1, next in
    let ws = tloc_line_span d ~first:(Textloc.first_byte ws) ~last:(next - 1) in
    d.pos <- next;
    d.line_pos <- line_pos;
    ws

  let dec_comment_newline d = (* [comment] newline, # checked *)
    let rec loop s max i =
      if i > max then i else match s.[i] with
      | '\n' -> i + 1
      | '\r' ->
          let i = i + 1 in
          if i > d.max_pos || d.s.[i] <> '\n'
          then dec_error_expected ~first:i d [Lf]
          else i + 1
      | '\x09' | '\x20' .. '\x7E' -> loop s max (i + 1)
      | '\x00' .. '\x08' | '\x0A' .. '\x1F' | '\x7F' ->
          dec_error_expected d ~first:i [Comment_char]
      | _ ->
          let udec = String.get_utf_8_uchar s i in
          match Uchar.utf_decode_is_valid udec with
          | true -> loop s max (i + Uchar.utf_decode_length udec)
          | false -> dec_error_expected d ~first:i [Comment_char]
    in
    let next = loop d.s d.max_pos d.pos in
    let line_pos =
      if next > d.max_pos then d.line_pos else
      fst d.line_pos + 1, next
    in
    let comment = tloc_line_span d ~first:d.pos ~last:(next - 1) in
    d.pos <- next;
    d.line_pos <- line_pos;
    comment

  let dec_ws_comment_newline d =
    if d.pos > d.max_pos then Textloc.none, Textloc.none else
    let ws = dec_ws d in
    if d.pos > d.max_pos then ws, Textloc.none else
    match d.s.[d.pos] with
    | '#' -> ws, dec_comment_newline d
    | '\n' | '\r' -> dec_newline d ws, Textloc.none
    | _ -> dec_error_expected d ~first:d.pos [Comment; Newline]

  let dec_table d ws = failwith "TODO"

  let dec_literal_string d = (* apostrophe checked. *)
    let rec loop s max i =
      if i > max then dec_error_unclosed d ~first:d.pos Literal_string else
      match s.[i] with
      | '\'' ->
          let first = d.pos + 1 and last = i - 1 in
          let s = String.sub s first (last - first + 1) in
          d.pos <- i + 1;
          s
      | '\x09' | '\x20' .. '\x26' | '\x28' .. '\x7E' -> loop s max (i + 1)
      | '\x00' .. '\x08' | '\x0A' .. '\x1F' | '\x7F' ->
          dec_error_expected d ~first:i [Literal_char]
      | _ ->
        let udec = String.get_utf_8_uchar s i in
        match Uchar.utf_decode_is_valid udec with
        | true -> loop s max (i + Uchar.utf_decode_length udec)
        | false -> dec_error_expected d ~first:i [Literal_char]
    in
    loop d.s d.max_pos (d.pos + 1)

  let rec dec_uchar_escape d ~first count u i = match count with
  | 0 ->
      if Uchar.is_valid u then Uchar.unsafe_of_int u, i else
      dec_error_escape d ~first ~last:(i - 1) (Not_uchar_value u)
  | count when i > d.max_pos ->
      dec_error_escape d ~first ~last:(i - 1) Invalid_uchar_escape
  | count ->
      match d.s.[i] with
      | '0' .. '9' as c ->
          let u = u * 16 + (Char.code c - 0x30) in
          dec_uchar_escape d ~first (count - 1) u (i + 1)
      | 'A' .. 'F' as c ->
          let u = u * 16 + (Char.code c - 0x37) in
          dec_uchar_escape d ~first (count - 1) u (i + 1)
      | c ->
          dec_error_escape d ~first ~last:(i - 1) Invalid_uchar_escape

  let dec_basic_string_with_escapes d first_esc =
    let flush b s max start i =
      if start <= max then Buffer.add_substring b s start (i - start)
    in
    let rec loop b s max start i =
      if i > max then dec_error_unclosed d ~first:d.pos Basic_string else
      match s.[i] with
      | '\"' ->
          flush b s max start i; d.pos <- i + 1; Buffer.contents b
      | '\x09' | '\x20' .. '\x21' | '\x23' .. '\x5B' | '\x5D' .. '\x7E' ->
          loop b s max start (i + 1)
      | '\x00' .. '\x08' | '\x0A' .. '\x1F' | '\x7F' ->
          dec_error_expected d ~first:i [Basic_char]
      | '\x5C' ->
          flush b s max start i;
          let i = i + 1 in
          if i > max then dec_error_unclosed d ~first:d.pos Basic_string else
          begin match s.[i] with
          | '\x22' | '\x5C' | '\x62' | '\x66' | '\x6E' | '\x72' | '\x74' as c ->
              let next = i + 1 in
              Buffer.add_char b c; loop b s max next next
          | '\x75' ->
              let u, next = dec_uchar_escape d ~first:(i - 1) 4 0 i in
              Buffer.add_utf_8_uchar b u; loop b s max next next
          | '\x55' ->
              let u, next = dec_uchar_escape d ~first:(i - 1) 8 0 i in
              Buffer.add_utf_8_uchar b u; loop b s max next next
          | c ->
              let first = i - 1 and last = i + 1 in
              dec_error_escape d ~first ~last (Not_escape_char c)
          end
      | _ ->
        let udec = String.get_utf_8_uchar s i in
        match Uchar.utf_decode_is_valid udec with
        | true -> loop b s max start (i + Uchar.utf_decode_length udec)
        | false -> dec_error_expected d ~first:i [Literal_char]
    in
    Buffer.reset d.buf;
    flush d.buf d.s d.max_pos d.pos first_esc;
    loop d.buf d.s d.max_pos first_esc first_esc

  let dec_basic_string d = (* quote checked *)
    let rec loop s max i =
      if i > max then dec_error_unclosed d ~first:d.pos Basic_string else
      match s.[i] with
      | '\"' ->
          let first = d.pos + 1 and last = i - 1 in
          let s = String.sub s first (last - first + 1) in
          d.pos <- i + 1;
          s
      | '\x09' | '\x20' .. '\x21' | '\x23' .. '\x5B' | '\x5D' .. '\x7E' ->
          loop s max (i + 1)
      | '\x00' .. '\x08' | '\x0A' .. '\x1F' | '\x7F' ->
          dec_error_expected d ~first:i [Basic_char]
      | '\x5C' ->
          dec_basic_string_with_escapes d i
      | _ ->
        let udec = String.get_utf_8_uchar s i in
        match Uchar.utf_decode_is_valid udec with
        | true -> loop s max (i + Uchar.utf_decode_length udec)
        | false -> dec_error_expected d ~first:i [Literal_char]
    in
    loop d.s d.max_pos d.pos

  let is_unquoted_key_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true | _ -> false

  let dec_unquoted_key d =
    let rec loop s max i =
      if i > max || not (is_unquoted_key_char s.[i]) then i else
      loop s max (i + 1)
    in
    let next = loop d.s d.max_pos d.pos in
    let first = d.pos and last = next - 1 in
    let s = String.sub d.s first (last - first + 1) in
    d.pos <- next;
    s

  let dec_simple_key d =
    if d.pos > d.max_pos then dec_error_expected d ~first:d.pos [Key] else
    match d.s.[d.pos] with
    | '\'' -> let s = dec_literal_string d in `Quoted_literal, s
    | '\"' -> let s = dec_basic_string d in `Quoted_basic, s
    | c when is_unquoted_key_char c -> `Bare, dec_unquoted_key d
    | _ -> dec_error_expected d ~first:d.pos [Key]

  let dec_key d ws =
    let rec loop d acc wsl =
      let k = dec_simple_key d in
      let wsr = dec_ws d in
      let acc = (wsl, k, wsr) :: acc in
      if d.pos > d.max_pos || d.s.[d.pos] <> '.' then List.rev acc else
      let ws = dec_ws d in
      loop d acc ws
    in
    loop d [] ws

  let dec_true d = (* 't' checked *)
    let last = d.pos + 3 in
    if last > d.max_pos then dec_error_eoi d ~first:d.pos [True] else
    if d.s.[d.pos + 1] = 'r' && d.s.[d.pos + 2] = 'u' && d.s.[d.pos + 3] = 'e'
    then (d.pos <- last + 1; Boolean true)
    else dec_error_expected d ~first:d.pos ~last [True]

  let dec_false d = (* 'f' checked *)
    let last = d.pos + 4 in
    if last > d.max_pos then dec_error_eoi d ~first:d.pos [False] else
    if d.s.[d.pos + 1] = 'a' && d.s.[d.pos + 2] = 'l' &&
       d.s.[d.pos + 3] = 's' && d.s.[d.pos + 4] = 'e'
    then (d.pos <- last + 1; Boolean false)
    else dec_error_expected d ~first:d.pos ~last [False]

  let parse_float d ~first ~last =
    (* [float_of_string] is compatible with what we accept. *)
    try Float (float_of_string (String.sub d.s first (last - first + 1))) with
    | Failure _ -> assert false

  let parse_int64 d ~first ~last =
    try Integer (Int64.of_string (String.sub d.s first (last - first + 1))) with
    | Failure _ ->
        (* [Int64.of_string] is compatible with what our parser checked. If
           we get here, it should be only because of overflow. *)
        dec_error_invalid d ~first ~last Int64_overflow

  let dec_inf d ~first = (* 'i' checked *)
    let last = d.pos + 2 in
    if last > d.max_pos then dec_error_eoi d ~first [Inf] else
    if d.s.[d.pos + 1] = 'n' && d.s.[d.pos + 2] = 'f'
    then (d.pos <- last + 1; parse_float d ~first ~last)
    else dec_error_expected d ~first ~last [Inf]

  let dec_nan d ~first = (* 'n' checked *)
    let last = d.pos + 2 in
    if last > d.max_pos then dec_error_eoi d ~first [Nan] else
    if d.s.[d.pos + 1] = 'a' && d.s.[d.pos + 2] = 'n'
    then (d.pos <- last + 1; parse_float d ~first ~last)
    else dec_error_expected d ~first ~last [Nan]

  let rec dec_float_exp d ~first = (* 'e' checked *)
    let rec loop d ~last_is_digit ~first pos =
      if pos > d.max_pos then parse_float d ~first ~last:(pos - 1) else
      match d.s.[pos] with
      | '0' .. '9' -> loop d ~last_is_digit:true ~first (pos + 1)
      | '_' when last_is_digit -> loop d ~last_is_digit:false ~first (pos + 1)
      | _ when last_is_digit ->
          d.pos <- pos; parse_float d ~first ~last:(pos - 1)
      | _ -> dec_error_expected d ~first:pos [Dec_digit]
    in
    let next = d.pos + 1 in
    if next > d.max_pos then dec_error_eoi d ~first [Float_exp] else
    match d.s.[next] with
    | '-' | '+' -> d.pos <- next; dec_float_exp d ~first
    | _ ->  loop d ~last_is_digit:false ~first next

  let dec_float_frac d ~first = (* '.' checked *)
    let rec loop d ~last_is_digit ~first pos =
      if pos > d.max_pos then parse_float d ~first ~last:(pos - 1) else
      match d.s.[pos] with
      | '0' .. '9' -> loop d ~last_is_digit:true ~first (pos + 1)
      | 'e' -> d.pos <- pos; dec_float_exp d ~first
      | '_' when last_is_digit -> loop d ~last_is_digit:false ~first (pos + 1)
      | _ when last_is_digit ->
          d.pos <- pos; parse_float d ~first ~last:(pos - 1)
      | _ -> dec_error_expected d ~first:pos [Dec_digit]
    in
    let next = d.pos + 1 in
    if next > d.max_pos then dec_error_eoi d ~first [Float_frac] else
    loop d ~last_is_digit:false ~first next

  let rec dec_dec_integer_or_float d ~first =
    if d.pos > d.max_pos then dec_error_eoi d ~first [Integer; Float] else
    match d.s.[d.pos] with
    | 'i' -> dec_inf d ~first
    | 'n' -> dec_nan d ~first
    | '0' ->
        let next = d.pos + 1 in
        begin match d.s.[next] with
        | '.' -> d.pos <- next; dec_float_frac d ~first
        | 'e' -> d.pos <- next; dec_float_exp d ~first
        | '0' .. '9' -> dec_error_invalid d ~first Leading_zeros
        | _ -> Integer 0L
        end
    | '1' .. '9' ->
        let rec loop d ~last_is_digit ~first pos =
          if pos > d.max_pos then parse_int64 d ~first ~last:(pos - 1) else
          match d.s.[pos] with
          | '0' .. '9' -> loop d ~last_is_digit:true ~first (pos + 1)
          | '.' -> d.pos <- pos; dec_float_frac d ~first
          | 'e' -> d.pos <- pos; dec_float_exp d ~first
          | '_' when last_is_digit ->
              loop d ~last_is_digit:false ~first (pos + 1)
          | _ when last_is_digit ->
              d.pos <- pos; parse_int64 d ~first ~last:(pos - 1)
          | _ -> dec_error_expected d ~first:pos [Dec_digit]
        in
        loop d ~last_is_digit:false ~first d.pos
    | _ -> dec_error_expected d ~first:d.pos [Integer; Float]

  let dec_hex_integer d ~first =
    if d.pos > d.max_pos then dec_error_eoi d ~first [Hex_digit] else
    let rec loop d ~last_is_digit ~first pos =
      if pos > d.max_pos then parse_int64 d ~first ~last:(pos - 1) else
      match d.s.[pos] with
      | '0' .. '9' | 'A' .. 'F' -> loop d ~last_is_digit:true ~first (pos + 1)
      | '_' when last_is_digit -> loop d ~last_is_digit:false ~first (pos + 1)
      | _ when last_is_digit ->
          d.pos <- pos; parse_int64 d ~first ~last:(pos - 1)
      | _ -> dec_error_expected d ~first:pos [Hex_digit]
    in
    loop d ~last_is_digit:false ~first d.pos

  let dec_oct_integer d ~first =
    if d.pos > d.max_pos then dec_error_eoi d ~first [Oct_digit] else
    let rec loop d ~last_is_digit ~first pos =
      if pos > d.max_pos then parse_int64 d ~first ~last:(pos - 1) else
      match d.s.[pos] with
      | '0' .. '7' -> loop d ~last_is_digit:true ~first (pos + 1)
      | '_' when last_is_digit -> loop d ~last_is_digit:false ~first (pos + 1)
      | _ when last_is_digit ->
          d.pos <- pos; parse_int64 d ~first ~last:(pos - 1)
      | _ -> dec_error_expected d ~first:pos [Oct_digit]
    in
    loop d ~last_is_digit:false ~first d.pos

  let dec_bin_integer d ~first =
    if d.pos > d.max_pos then dec_error_eoi d ~first [Bin_digit] else
    let rec loop d ~last_is_digit ~first pos =
      if pos > d.max_pos then parse_int64 d ~first ~last:(pos - 1) else
      match d.s.[pos] with
      | '0' .. '1' -> loop d ~last_is_digit:true ~first (pos + 1)
      | '_' when last_is_digit -> loop d ~last_is_digit:false ~first (pos + 1)
      | _ when last_is_digit ->
          d.pos <- pos; parse_int64 d ~first ~last:(pos - 1)
      | _ -> dec_error_expected d ~first:pos [Bin_digit]
    in
    loop d ~last_is_digit:false ~first d.pos

  let dec_integer_or_float d ~first = (* first digit checked *)
    d.pos <- d.pos + 1;
    if d.pos > d.max_pos then parse_int64 d ~first ~last:(d.pos - 1) else
    match d.s.[d.pos] with
    | 'x' -> d.pos <- d.pos + 1; dec_hex_integer d ~first
    | 'o' -> d.pos <- d.pos + 1; dec_oct_integer d ~first
    | 'b' -> d.pos <- d.pos + 1; dec_bin_integer d ~first
    | '0' .. '9' -> dec_dec_integer_or_float d ~first
    | _ -> dec_error_expected d ~first ~last:d.pos [Integer]

  let n_digits d ~n =
    let rec loop d n i v =
      if n = 0 then (d.pos <- i; v) else
      if i > d.max_pos then dec_error_eoi d ~first:i [Dec_digit] else
      match d.s.[i] with
      | '0' .. '9' as c ->
          loop d (n - 1) (i + 1) (v * 10 + (Char.code c - 0x30))
      | _ -> dec_error_expected d ~first:i [Dec_digit]
    in
    loop d n d.pos 0

  let dec_char d ~c =
    if d.pos > d.max_pos then dec_error_eoi d ~first:d.pos [Char c] else
    if d.s.[d.pos] = c then (d.pos <- d.pos + 1) else
    dec_error_expected d ~first:d.pos [Char c]

  let dec_full_date d =
    let first = d.pos in
    let yyyy = n_digits d ~n:4 in
    dec_char d ~c:'-';
    let mm = n_digits d ~n:2 in
    dec_char d ~c:'-';
    let dd = n_digits d ~n:2 in
    let date = yyyy, mm, dd in
    if is_date_valid date then date else
    dec_error_invalid d ~first ~last:(d.pos - 1) (Date date)

  let dec_hour d =
    let first = d.pos in
    let hh = n_digits d ~n:2 in
    if 00 <= hh && hh <= 23 then hh else
    dec_error_invalid d ~first ~last:(d.pos - 1) (Hour hh)

  let dec_minute d =
    let first = d.pos in
    let mm = n_digits d ~n:2 in
    if 00 <= mm && mm <= 59 then mm else
    dec_error_invalid d ~first ~last:(d.pos - 1) (Minute mm)

  let dec_sec d =
    let first = d.pos in
    let ss = n_digits d ~n:2 in
    if 00 <= ss && ss <= 60 then ss else
    dec_error_invalid d ~first ~last:(d.pos - 1) (Second ss)

  let dec_secfrac d = (* . checked *)
    d.pos <- d.pos + 1;
    let rec loop d first i =
      if i > d.max_pos ||
         match d.s.[i] with '0' .. '9' -> false | _ -> true
      then (float_of_string ("0." ^ String.sub d.s first (i - first)))
      else loop d first (i + 1)
    in
    loop d d.pos d.pos

  let dec_partial_time d =
    let hh = dec_hour d in
    dec_char d ~c:':';
    let mm = dec_minute d in
    dec_char d ~c:':';
    let ss = n_digits d ~n:2 in
    match d.s.[d.pos] with
    | '.' -> (hh, mm, ss), Some (dec_secfrac d)
    | _ -> (hh, mm, ss), None

  let dec_time_offset d =
    if d.pos > d.max_pos then dec_error_eoi d ~first:d.pos [Tz_offset] else
    match d.s.[d.pos] with
    | 'Z' -> d.pos <- d.pos + 1; 0
    | '+' | '-' as sign ->
        d.pos <- d.pos + 1;
        let hh = dec_hour d in
        dec_char d ~c:':';
        let mm = dec_minute d in
        (if sign = '+' then 1 else -1) * (hh * 3600 + mm * 60)
    | _ ->
        dec_error_eoi d ~first:d.pos [Tz_offset]

  let dec_full_time d =
    let time, frac = dec_partial_time d in
    time, frac, Some (dec_time_offset d)

  let dec_local_time d =
    let time, frac = dec_partial_time d in
    Date_time (None, Some (time, frac, None))

  let dec_date_time d =
    let date = dec_full_date d in
    if d.pos > d.max_pos then Date_time (Some date, None) else
    match d.s.[d.pos] with
    | 'T' ->
        d.pos <- d.pos + 1;
        let time = dec_full_time d in
        Date_time (Some date, Some time)
    | ' ' ->
        d.pos <- d.pos + 1;
        begin match d.s.[d.pos] with
        | '0' .. '9' -> Date_time (Some date, Some (dec_full_time d))
        | _ -> Date_time (Some date, None)
        end
    | _ ->
        Date_time (Some date, None)

  let dec_val d =
    if d.pos > d.max_pos then dec_error_expected d ~first:d.pos [Value] else
    match d.s.[d.pos] with
    | '+' | '-' ->
        let first = d.pos in
        d.pos <- d.pos + 1; dec_dec_integer_or_float d ~first
    | 'i' -> dec_inf d ~first:d.pos
    | 'n' -> dec_nan d ~first:d.pos
    | '0' .. '9' ->
        let colon_pos = d.pos + 2 in
        if colon_pos > d.max_pos then dec_integer_or_float d ~first:d.pos else
        if d.s.[colon_pos] = ':' then dec_local_time d else
        let dash_pos = d.pos + 4 in
        if dash_pos > d.max_pos then dec_integer_or_float d ~first:d.pos else
        if d.s.[dash_pos] = '-' then dec_date_time d else
        dec_integer_or_float d ~first:d.pos
    | '\'' -> failwith "TODO"
    | '\"' -> failwith "TODO"
    | 't' -> dec_true d
    | 'f' -> dec_false d
    | '[' -> failwith "TODO"
    | _ -> dec_error_expected d ~first:d.pos [Value]

  let dec_keyval_sep d =
    if d.pos > d.max_pos || d.s.[d.pos] <> '='
    then dec_error_expected d ~first:d.pos [Equal_char]
    else dec_ws d

  let dec_keyval d ws =
    let key = dec_key d ws in
    let ws = dec_keyval_sep d in
    let v = dec_val d in
    key, ws, v

  let dec_keyval_expression d ws =
    let kv = dec_keyval d ws in
    Keyval' (kv, dec_ws_comment_newline d)

  let dec_expression d =
    let ws = dec_ws d in
    if d.pos > d.max_pos then Whitespace (ws, Textloc.none) else
    match d.s.[d.pos] with
    | '#' -> Whitespace (ws, dec_comment_newline d)
    | '\n' | '\r' -> Whitespace (dec_newline d ws, Textloc.none)
    | '[' -> dec_table d ws
    | '\'' | '\"' -> dec_keyval_expression d ws
    | c when is_unquoted_key_char c -> dec_keyval_expression d ws
    | _ ->
        let exps = Error.[Keyval; Table; Comment; Newline; Eoi] in
        dec_error_expected d ~first:d.pos exps

  let dec_toml d =
    let rec loop d acc =
      if d.pos > d.max_pos then List.rev acc else
      loop d (dec_expression d :: acc)
    in
    loop d []

  let of_string ?layout ?locs ?file i =
    let d = decoder ?layout ?locs ?file i in
    match dec_toml d with
    | _ -> Ok ([], Meta.none)
    | exception Err (err, loc) -> Error (err, loc)

  let of_string' ?pp_error ?file i =
    Error.to_string ?pp:pp_error (of_string ?file i)

  let to_string t = failwith "TODO"


  (* FIXME cut and paste from sexp *)

  (* Indices *)

  type index = Nth of int | Key of string

  let pp_key = Format.pp_print_string
  let pp_index ?(pp_key = pp_key) () ppf = function
  | Nth n -> pf ppf "[%d]" n
  | Key k -> pp_key ppf k

  let pp_bracketed_index ?(pp_key = pp_key) () ppf = function
  | Nth n -> pf ppf "[%d]" n
  | Key k -> pf ppf "[%a]" pp_key k

  (* Paths *)

  type path = index list (* reversed *)

  let path_err i fmt = Format.kasprintf failwith ("%d: " ^^ fmt) i
  let path_err_unexp_eoi i = path_err i "unexpected end of input"
  let path_err_unexp_char i s = path_err i "unexpected character: %C" s.[i]
  let path_err_illegal_char i s = path_err i "illegal character here: %C" s.[i]
  let err_unexp i s =
    path_err i "unexpected input: %S" (Textdec.string_subrange ~first:i s)

  let path_parse_eoi s i max = if i > max then () else err_unexp i s
  let path_parse_index p s i max =
    let first, stop = match s.[i] with '[' -> i + 1, ']' | _ -> i, '.' in
    let last, next =
      let rec loop stop s i max = match i > max with
      | true -> if stop = ']' then path_err_unexp_eoi i else (i - 1), i
      | false ->
          let illegal = s.[i] = '[' || (s.[i] = ']' && stop = '.') in
          if illegal then path_err_illegal_char i s else
          if s.[i] <> stop then loop stop s (i + 1) max else
          (i - 1), if stop = ']' then i + 1 else i
      in
      loop stop s first max
    in
    let idx = Textdec.string_subrange ~first ~last s in
    if idx = "" then path_err first "illegal empty index" else
    match int_of_string idx with
    | exception Failure _ -> next, (Key idx) :: p
    | idx -> next, (Nth idx) :: p

  let path_of_string s =
    let rec loop p s i max =
      if i > max then p else
      let next, p = path_parse_index p s i max in
      if next > max then p else
      if s.[next] <> '.' then path_err_unexp_char next s else
      if next + 1 <= max then loop p s (next + 1) max else
      path_err_unexp_eoi next
    in
    try
      if s = "" then Ok [] else
      let start = if s.[0] = '.' then 1 else 0 in
      Ok (loop [] s start (String.length s - 1))
    with Failure e -> Error e

  let pp_path ?pp_key () ppf is =
    let pp_sep ppf () = Format.pp_print_char ppf '.' in
    Format.pp_print_list ~pp_sep (pp_index ?pp_key ()) ppf (List.rev is)

  (* Carets *)

  type caret_loc = Before | Over | After
  type caret = caret_loc * path

  let caret_path (_, p) = p
  let caret_of_string s =
    let rec loop p s i max =
      if i > max then Over, p else
      let next = i + 1 in
      match s.[i] with
      | 'v' when next <= max && s.[next] = '[' ->
          let next, p = path_parse_index p s next max in
          path_parse_eoi s next max; Before, p
      | c ->
          let next, p = path_parse_index p s i max in
          if next > max then Over, p else
          if s.[next] = 'v'
          then (path_parse_eoi s (next + 1) max; After, p) else
          if s.[next] <> '.' then path_err_unexp_char next s else
          if next + 1 <= max then loop p s (next + 1) max else
          path_err_unexp_eoi next
    in
    try
      if s = "" then Ok (Over, []) else
      let start = if s.[0] = '.' then 1 else 0 in
      Ok (loop [] s start (String.length s - 1))
    with Failure e -> Error e

  let pp_caret ?pp_key () ppf = function
  | Over, p -> (pp_path ?pp_key ()) ppf p
  | Before, (c :: p) ->
      (pp_path ?pp_key ()) ppf p;
      (if p <> [] then Format.pp_print_char ppf '.');
      Format.pp_print_char ppf 'v'; (pp_bracketed_index ?pp_key ()) ppf c
  | After, (c :: p) ->
      (pp_path ?pp_key ()) ppf p;
      (if p <> [] then Format.pp_print_char ppf '.');
      (pp_bracketed_index ?pp_key ()) ppf c; Format.pp_print_char ppf 'v'
  | _ -> ()
end
