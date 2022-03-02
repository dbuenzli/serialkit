(*---------------------------------------------------------------------------
   Copyright (c) 2019 The serialkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)


let unsafe_get = String.unsafe_get
module String_set = Set.Make (String)

(* Heterogeneous dictionaries *)

module Dict = struct
  (* Type identifiers, can be deleted once we require 5.1 *)
  module Type = struct
    type (_, _) eq = Equal : ('a, 'a) eq
    module Id = struct
      type _ id = ..
      module type ID = sig type t type _ id += Id : t id end
      type 'a t = (module ID with type t = 'a)

      let make (type a) () : a t =
        (module struct type t = a type _ id += Id : t id end)

      let provably_equal
          (type a b) ((module A) : a t) ((module B) : b t) : (a, b) eq option
        =
        match A.Id with B.Id -> Some Equal | _ -> None

      let uid (type a) ((module A) : a t) =
        Obj.Extension_constructor.id (Obj.Extension_constructor.of_val A.Id)
    end
  end

  module M = Map.Make (Int)
  type 'a key = 'a Type.Id.t
  type binding = B : 'a key * 'a -> binding
  type t = binding M.t

  let key = Type.Id.make
  let empty = M.empty
  let mem k m = M.mem (Type.Id.uid k) m
  let add k v m = M.add (Type.Id.uid k) (B (k, v)) m
  let tag k m = add k () m
  let remove k m = M.remove (Type.Id.uid k) m
  let find : type a. a key -> t -> a option =
  fun k m -> match M.find_opt (Type.Id.uid k) m with
  | None -> None
  | Some B (k', v) ->
      match Type.Id.provably_equal k k' with
      | None -> assert false | Some Type.Equal -> Some v
end

(* Text locations *)

module Textloc = struct

  (* File paths *)

  type fpath = string
  let file_none = "-"
  let pp_path = Format.pp_print_string

  (* Byte positions *)

  type byte_pos = int (* zero-based *)
  let byte_pos_none = -1

  (* Lines *)

  type line_num = int (* one-based *)
  let line_num_none = -1

  (* Line positions

     We keep the byte position of the first element on the line. This
     first element may not exist and be equal to the text length if
     the input ends with a newline. Editors expect tools to compute
     visual columns (not a very good idea). By keeping these byte
     positions we can approximate columns by subtracting the line byte
     position data byte location. This will only be correct on
     US-ASCII data. *)

  type line_pos = line_num * byte_pos
  let line_pos_first = 1, 0
  let line_pos_none = line_num_none, byte_pos_none

  (* Text locations *)

  type t =
    { file : fpath;
      first_byte : byte_pos; last_byte : byte_pos;
      first_line : line_pos; last_line : line_pos }

  let v ~file ~first_byte ~last_byte ~first_line ~last_line =
    { file; first_byte; last_byte; first_line; last_line }

  let file l = l.file
  let first_byte l = l.first_byte
  let last_byte l = l.last_byte
  let first_line l = l.first_line
  let last_line l = l.last_line
  let none =
    let first_byte = byte_pos_none and last_byte = byte_pos_none in
    let first_line = line_pos_none and last_line = line_pos_none in
    v ~file:file_none ~first_byte ~last_byte ~first_line ~last_line

  (* Predicates and comparisons *)

  let is_none l = l.first_byte < 0
  let is_empty l = l.first_byte > l.last_byte
  let equal l0 l1 =
    String.equal l0.file l1.file &&
    Int.equal l0.first_byte l1.first_byte &&
    Int.equal l0.last_byte l1.last_byte

  let compare l0 l1 =
    let c = String.compare l0.file l1.file in
   if c <> 0 then c else
    let c = Int.compare l0.first_byte l1.first_byte in
    if c <> 0 then c else
    Int.compare l0.last_byte l1.last_byte

  (* Shrink and stretch *)

  let set_first l ~first_byte ~first_line = { l with first_byte; first_line }
  let set_last l ~last_byte ~last_line = { l with last_byte; last_line }

  [@@@warning "-6"]
  let to_first l = v l.file l.first_byte l.first_byte l.first_line l.first_line
  let to_last l = v l.file l.last_byte l.last_byte l.last_line l.last_line
  let before l = v l.file l.first_byte byte_pos_none l.first_line line_pos_none
  let after l =
    v l.file (l.first_byte + 1) byte_pos_none l.last_line line_pos_none
  [@@@warning "+6"]

  let span l0 l1 =
    let first_byte, first_line =
      if l0.first_byte < l1.first_byte
      then l0.first_byte, l0.first_line
      else l1.first_byte, l1.first_line
    in
    let last_byte, last_line, file =
      if l0.last_byte < l1.last_byte
      then l1.last_byte, l1.last_line, l1.file
      else l0.last_byte, l0.last_line, l0.file
    in
    v ~file ~first_byte ~first_line ~last_byte ~last_line

  [@@@warning "-6"]
  let reloc ~first ~last =
    v last.file first.first_byte last.last_byte first.first_line last.last_line
  [@@@warning "+6"]
  (* Formatters *)

  let pf = Format.fprintf
  let pp_ocaml ppf l = match is_none l with
  | true -> pf ppf "File \"%a\"" pp_path l.file
  | false ->
      let pp_lines ppf l = match fst l.first_line = fst l.last_line with
      | true -> pf ppf "line %d" (fst l.first_line)
      | false -> pf ppf "lines %d-%d" (fst l.first_line) (fst l.last_line)
      in
      (* "characters" represent positions (insertion points) not columns *)
      let pos_s = l.first_byte - snd l.first_line in
      let pos_e = l.last_byte - snd l.last_line + 1 in
      if pos_s = 0 && pos_e = 0
      then pf ppf "File \"%a\", %a" pp_path l.file pp_lines l
      else pf ppf "File \"%a\", %a, characters %d-%d"
          pp_path l.file pp_lines l pos_s pos_e

  let pp_gnu ppf l = match is_none l with
  | true -> pf ppf "%a:" pp_path l.file
  | false ->
      let pp_lines ppf l =
        let col_s = l.first_byte - snd l.first_line + 1 in
        let col_e = l.last_byte - snd l.last_line + 1 in
        match fst l.first_line = fst l.last_line with
        | true ->  pf ppf "%d.%d-%d" (fst l.first_line) col_s col_e
        | false ->
            pf ppf "%d.%d-%d.%d"
              (fst l.first_line) col_s (fst l.last_line) col_e
      in
      pf ppf "%a:%a" pp_path l.file pp_lines l

  let pp = pp_gnu

  let pp_dump ppf l =
    pf ppf "file:%s bytes:%d-%d lines:%d-%d lines-bytes:%d-%d]"
      l.file l.first_byte l.last_byte (fst l.first_line) (fst l.last_line)
      (snd l.first_line) (snd l.last_line)
end

(* Node meta data *)

module Meta = struct
  type id = int
  type t = { textloc : Textloc.t; id : id; dict : Dict.t }

  let new_id = let id = Atomic.make 0 in fun () -> Atomic.fetch_and_add id 1
  let make ?(textloc = Textloc.none) () =
    { textloc; id = new_id (); dict = Dict.empty }

  let none = make ()
  let id m = m.id
  let textloc m = m.textloc
  let with_textloc ~keep_id m textloc = match keep_id with
  | true -> { m with textloc }
  | false -> { m with textloc; id = new_id () }

  let equal m0 m1 = Int.equal m0.id m1.id
  let compare m0 m1 = Int.compare m0.id m1.id
  let is_none m = equal none m

  type 'a key = 'a Dict.key
  let key = Dict.key
  let mem k m = Dict.mem k m.dict
  let add k v m = { m with dict = Dict.add k v m.dict }
  let tag k m = add k () m
  let remove k m = { m with dict = Dict.remove k m.dict }
  let find k m = Dict.find k m.dict
end

(* UTF tools

   This can be killed once we require OCaml 4.14.
   This is stricly equivalent to what was upstream to the Stdlib. *)

module Uchar = struct
  include Uchar

  (* UTF codecs tools *)

  type utf_decode = int
  (* This is an int [0xDUUUUUU] decomposed as follows:
     - [D] is four bits for decode information, the highest bit is set if the
     decode is valid. The three lower bits indicate the number of elements
     from the source that were consumed by the decode.
     - [UUUUUU] is the decoded Unicode character or the Unicode replacement
     character U+FFFD if for invalid decodes. *)

  let valid_bit = 27
  let decode_bits = 24

  let[@inline] utf_decode_is_valid d = (d lsr valid_bit) = 1
  let[@inline] utf_decode_length d = (d lsr decode_bits) land 0b111
  let[@inline] utf_decode_uchar d = unsafe_of_int (d land 0xFFFFFF)
  let[@inline] utf_decode n u = ((8 lor n) lsl decode_bits) lor (to_int u)
  let[@inline] utf_decode_invalid n = (n lsl decode_bits) lor (Uchar.to_int rep)
end

(* UTF-8 *)

let dec_invalid = Uchar.utf_decode_invalid
let[@inline] dec_ret n u = Uchar.utf_decode n (Uchar.unsafe_of_int u)

let[@inline] not_in_x80_to_xBF b = b lsr 6 <> 0b10
let[@inline] not_in_xA0_to_xBF b = b lsr 5 <> 0b101
let[@inline] not_in_x80_to_x9F b = b lsr 5 <> 0b100
let[@inline] not_in_x90_to_xBF b = b < 0x90 || 0xBF < b
let[@inline] not_in_x80_to_x8F b = b lsr 4 <> 0x8

let[@inline] utf_8_uchar_2 b0 b1 =
  ((b0 land 0x1F) lsl 6) lor
  ((b1 land 0x3F))

let[@inline] utf_8_uchar_3 b0 b1 b2 =
  ((b0 land 0x0F) lsl 12) lor
  ((b1 land 0x3F) lsl 6) lor
  ((b2 land 0x3F))

let[@inline] utf_8_uchar_4 b0 b1 b2 b3 =
  ((b0 land 0x07) lsl 18) lor
  ((b1 land 0x3F) lsl 12) lor
  ((b2 land 0x3F) lsl 6) lor
  ((b3 land 0x3F))

external unsafe_get_uint8 : string -> int -> int = "%bytes_unsafe_get"

let string_get_utf_8_uchar b i =
  let b0 = Bytes.get_uint8 (Bytes.unsafe_of_string b) i in
  let get = unsafe_get_uint8 in
  let max = String.length b - 1 in
  match Char.unsafe_chr b0 with (* See The Unicode Standard, Table 3.7 *)
  | '\x00' .. '\x7F' -> dec_ret 1 b0
  | '\xC2' .. '\xDF' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
      let b1 = get b i in if not_in_x80_to_xBF b1 then dec_invalid 1 else
      dec_ret 2 (utf_8_uchar_2 b0 b1)
  | '\xE0' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
      let b1 = get b i in if not_in_xA0_to_xBF b1 then dec_invalid 1 else
      let i = i + 1 in if i > max then dec_invalid 2 else
      let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
      dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
  | '\xE1' .. '\xEC' | '\xEE' .. '\xEF' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
      let b1 = get b i in if not_in_x80_to_xBF b1 then dec_invalid 1 else
      let i = i + 1 in if i > max then dec_invalid 2 else
      let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
      dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
  | '\xED' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
      let b1 = get b i in if not_in_x80_to_x9F b1 then dec_invalid 1 else
      let i = i + 1 in if i > max then dec_invalid 2 else
      let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
      dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
  | '\xF0' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
      let b1 = get b i in if not_in_x90_to_xBF b1 then dec_invalid 1 else
      let i = i + 1 in if i > max then dec_invalid 2 else
      let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
      let i = i + 1 in if i > max then dec_invalid 3 else
      let b3 = get b i in if not_in_x80_to_xBF b3 then dec_invalid 3 else
      dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
  | '\xF1' .. '\xF3' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
      let b1 = get b i in if not_in_x80_to_xBF b1 then dec_invalid 1 else
      let i = i + 1 in if i > max then dec_invalid 2 else
      let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
      let i = i + 1 in if i > max then dec_invalid 3 else
      let b3 = get b i in if not_in_x80_to_xBF b3 then dec_invalid 3 else
      dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
  | '\xF4' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
      let b1 = get b i in if not_in_x80_to_x8F b1 then dec_invalid 1 else
      let i = i + 1 in if i > max then dec_invalid 2 else
      let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
      let i = i + 1 in if i > max then dec_invalid 3 else
      let b3 = get b i in if not_in_x80_to_xBF b3 then dec_invalid 3 else
      dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
  | _ -> dec_invalid 1


(* Error message helpers. *)

module Err_msg = struct
  let pf = Format.fprintf
  let pp_sp = Format.pp_print_space
  let pp_nop _ () = ()
  let pp_any fmt ppf _ = pf ppf fmt
  let pp_op_enum op ?(empty = pp_nop) pp_v ppf = function
  | [] -> empty ppf ()
  | [v] -> pp_v ppf v
  | _ as vs ->
      let rec loop ppf = function
      | [v0; v1] -> pf ppf "%a@ %s@ %a" pp_v v0 op pp_v v1
      | v :: vs -> pf ppf "%a,@ " pp_v v; loop ppf vs
      | [] -> assert false
      in
      loop ppf vs

  let pp_and_enum ?empty pp_v ppf vs = pp_op_enum "and" ?empty pp_v ppf vs
  let pp_or_enum ?empty pp_v ppf vs = pp_op_enum "or" ?empty pp_v ppf vs
  let pp_did_you_mean pp_v ppf = function
  | [] -> () | vs -> pf ppf "Did@ you@ mean %a ?" (pp_or_enum pp_v) vs

  let pp_must_be pp_v ppf = function
  | [] -> () | vs -> pf ppf "Must be %a." (pp_or_enum pp_v) vs

  let pp_unknown ~kind pp_v ppf v = pf ppf "Unknown %a %a." kind () pp_v v
  let pp_unknown' ~kind pp_v ~hint ppf (v, hints) = match hints with
  | [] -> pp_unknown ~kind pp_v ppf v
  | hints -> pp_unknown ~kind pp_v ppf v; pp_sp ppf (); (hint pp_v) ppf hints

  let edit_distance s0 s1 =
    let minimum (a : int) (b : int) (c : int) : int = min a (min b c) in
    let s0,s1 = if String.length s0 <= String.length s1 then s0,s1 else s1,s0 in
    let m = String.length s0 and n = String.length s1 in
    let rec rows row0 row i = match i > n with
    | true -> row0.(m)
    | false ->
        row.(0) <- i;
        for j = 1 to m do
          if s0.[j - 1] = s1.[i - 1] then row.(j) <- row0.(j - 1) else
          row.(j) <- minimum (row0.(j - 1) + 1) (row0.(j) + 1) (row.(j - 1) + 1)
        done;
        rows row row0 (i + 1)
    in
    rows (Array.init (m + 1) (fun x -> x)) (Array.make (m + 1) 0) 1

  let suggest ?(dist = 2) candidates s =
    let add (min, acc) name =
      let d = edit_distance s name in
      if d = min then min, (name :: acc) else
      if d < min then d, [name] else
      min, acc
    in
    let d, suggs = List.fold_left add (max_int, []) candidates in
    if d <= dist (* suggest only if not too far *) then List.rev suggs else []
end

(* UTF-8 decoding table. *)

module Utf_8 = struct
  type case =
  | L1 | L2 | L3_E0 | L3_E1_EC_or_EE_EF | L3_ED | L4_F0 | L4_F1_F3 | L4_F4 | E

  let case =
(*
  (* See https://tools.ietf.org/html/rfc3629#section-4 *)
  Printf.printf "[|";
  for i = 0 to 255 do
    if i mod 16 = 0 then Printf.printf "\n";
    if 0x00 <= i && i <= 0x7F then Printf.printf "L1; " else
    if 0xC2 <= i && i <= 0xDF then Printf.printf "L2; " else
    if 0xE0 = i then Printf.printf "L3_E0; " else
    if 0xE1 <= i && i <= 0xEC || 0xEE <= i && i <= 0xEF
    then Printf.printf "L3_E1_EC_or_EE_EF; " else
    if 0xED = i then Printf.printf "L3_ED;" else
    if 0xF0 = i then Printf.printf "L4_F0; " else
    if 0xF1 <= i && i <= 0xF3 then Printf.printf "L4_F1_F3; " else
    if 0xF4 = i then Printf.printf "L4_F4; " else
    Printf.printf "E; "
  done;
  Printf.printf "\n|]"
*)
  [|
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    E; E; E; E; E; E; E; E; E; E; E; E; E; E; E; E;
    E; E; E; E; E; E; E; E; E; E; E; E; E; E; E; E;
    E; E; E; E; E; E; E; E; E; E; E; E; E; E; E; E;
    E; E; E; E; E; E; E; E; E; E; E; E; E; E; E; E;
    E; E; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2;
    L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2;
    L3_E0; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF;
    L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF;
    L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF;
    L3_E1_EC_or_EE_EF; L3_ED;L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF;
    L4_F0; L4_F1_F3; L4_F1_F3; L4_F1_F3; L4_F4; E; E; E; E; E; E; E; E; E; E; E;
  |]
end

(* UTF-8 text decoder *)

module Textdec = struct
  type 'a fmt = Format.formatter -> 'a -> unit
  let pp_did_you_mean = Err_msg.pp_did_you_mean
  let pp_and_enum = Err_msg.pp_and_enum
  let pp_or_enum = Err_msg.pp_or_enum
  let pp_did_you_mean = Err_msg.pp_did_you_mean
  let pp_must_be = Err_msg.pp_must_be
  let pp_unknown = Err_msg.pp_unknown
  let pp_unknown' = Err_msg.pp_unknown'

  (* Decoders *)

  type t =
    { file : Textloc.fpath; i : string; tok : Buffer.t;
      mutable pos : int; mutable line : int; mutable line_pos : int; }

  let from_string ?(file = Textloc.file_none) i =
    { file; i; tok = Buffer.create 255; pos = 0; line = 1; line_pos = 0 }

  (* Location *)

  let file d = d.file
  let pos d = d.pos
  let line d = d.line, d.line_pos
  let loc d ~first_byte ~last_byte ~first_line ~last_line =
    Textloc.v ~file:d.file ~first_byte ~last_byte ~first_line ~last_line

  let loc_to_here d ~first_byte ~first_line =
    let last_line = (d.line, d.line_pos) in
    loc d ~first_byte ~last_byte:d.pos ~first_line ~last_line

  let loc_here d =
    loc_to_here d ~first_byte:d.pos ~first_line:(d.line, d.line_pos)

  (* Errors *)

  exception Err of Textloc.t * string

  let err loc msg = raise_notrace (Err (loc, msg))
  let err_to_here d ~first_byte ~first_line fmt =
    Format.kasprintf (err (loc_to_here d ~first_byte ~first_line)) fmt

  let err_here d fmt = Format.kasprintf (err (loc_here d)) fmt
  let err_suggest = Err_msg.suggest

  (* Lexing *)

  let[@inline] incr_line d = match d.i.[d.pos] with (* assert (not (eoi d)) *)
  | '\r' -> d.line <- d.line + 1; d.line_pos <- d.pos + 1
  | '\n' ->
      (if d.pos = 0 || d.i.[d.pos - 1] <> '\r' then d.line <- d.line + 1);
      d.line_pos <- d.pos + 1;
  | _ -> ()

  let[@inline] eoi d = d.pos >= String.length d.i
  let[@inline] byte d = if eoi d then 0xFFFF else Char.code d.i.[d.pos]
  let[@inline] accept_byte d = incr_line d; d.pos <- d.pos + 1

  let accept_utf_8 accept d =
    let err d = match byte d with
    | 0xFFFF -> err_here d "UTF-8 decoding error: unexpected end of input"
    | b -> err_here d "UTF-8 decoding error: byte %02x illegal here" b
    in
    let accept_tail d = if (byte d lsr 6 = 0b10) then accept d else err d in
    match byte d with
    | 0xFFFF -> err d
    | b ->
        (* If a subsequent [byte d] invocation is 0xFFFF we get to [err]. *)
        match Utf_8.case.(b) with
        | L1 -> accept d
        | L2 -> accept d; accept_tail d
        | L3_E0 ->
            accept d;
            if (byte d - 0xA0 < 0xBF - 0xA0) then accept d else err d;
            accept_tail d
        | L3_E1_EC_or_EE_EF ->
            accept d; accept_tail d; accept_tail d
        | L3_ED ->
            accept d;
            if (byte d - 0x80 < 0x9F - 0x80) then accept d else err d;
            accept_tail d
        | L4_F0 ->
            accept d;
            if (byte d - 0x90 < 0xBF - 0x90) then accept d else err d;
            accept_tail d; accept_tail d
        | L4_F1_F3 ->
            accept d;
            accept_tail d; accept_tail d; accept_tail d;
        | L4_F4 ->
            accept d;
            if (byte d - 0x80 < 0x8F - 0x80) then accept d else err d;
        | E -> err d

  let accept_uchar d = accept_utf_8 accept_byte d

  (* Tokenizer *)

  let[@inline] lex_clear d = Buffer.clear d.tok
  let[@inline] lex_pop d = let t = Buffer.contents d.tok in lex_clear d; t
  let[@inline] lex_add_byte d b = Buffer.add_char d.tok (Char.chr b)
  let[@inline] lex_add_bytes d s = Buffer.add_string d.tok s
  let[@inline] lex_add_char d c = Buffer.add_char d.tok c
  let[@inline] lex_add_uchar d u = Buffer.add_utf_8_uchar d.tok u
  let[@inline] lex_accept_byte d =
    Buffer.add_char d.tok d.i.[d.pos]; accept_byte d

  let[@inline] lex_accept_uchar d = accept_utf_8 lex_accept_byte d

  (* Insertions and substitutions *)

  let string_subrange ?(first = 0) ?last s =
    let max = String.length s - 1 in
    let last = match last with
    | None -> max
    | Some l when l > max -> max
    | Some l -> l
    in
    let first = if first < 0 then 0 else first in
    if first > last then "" else
    String.sub s first (last - first + 1)

  let string_replace ~start ~stop ~rep s =
    let len = String.length s in
    if stop < start || start < 0 || start > len || stop < 0 || stop > len
    then invalid_arg (Printf.sprintf "invalid start:%d stop:%d" start stop) else
    let b = String.sub s 0 start in
    let a = String.sub s stop (len - stop) in
    String.concat "" [b; rep; a]
end
