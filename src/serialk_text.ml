(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

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

(* Text locations *)

module Tloc = struct

  (* File paths *)

  type fpath = string
  let no_file = "-"
  let pp_path = Format.pp_print_string

  (* Byte and line positions. *)

  type pos = int
  type line = int
  type line_pos = line * pos
  (* For lines we keep the byte position just after the newlines. It
     seems editors are still expecting tools to compute visual columns
     which is stupid. By keeping these byte positions we can
     approximate columns by subtracting the line byte position from
     the byte location. This will only be correct on US-ASCII
     data. Best would be to be able to give them byte ranges directly. *)

  (* Text locations *)

  type t =
    { file : fpath;
      first_byte : pos; first_line : pos * line;
      last_byte : pos; last_line : pos * line }

  let v ~file ~first_byte ~first_line ~last_byte ~last_line =
    { file; first_byte; last_byte; first_line; last_line }

  let file l = l.file
  let first_byte l = l.first_byte
  let last_byte l = l.last_byte
  let first_line l = l.first_line
  let last_line l = l.last_line
  let nil =
    let pnil = -1 and lnil = (-1, -1) in
    v "" pnil lnil pnil lnil

  let to_first l =
    v l.file l.first_byte l.first_line l.first_byte l.first_line

  let to_last l =
    v l.file l.last_byte l.last_line l.last_byte l.last_line

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
    v ~file:l0.file ~first_byte ~first_line ~last_byte ~last_line

  let reloc ~first ~last =
    v last.file first.first_byte first.first_line last.last_byte last.last_line

  (* Formatters *)

  let pf = Format.fprintf
  let pp_ocaml ppf l = match l.last_byte < 0 with
  | true -> pf ppf "File \"%a\", line n/a, characters n/a" pp_path l.file
  | false ->
      let pp_lines ppf l = match fst l.first_line = fst l.last_line with
      | true -> pf ppf "line %d" (fst l.first_line)
      | false -> pf ppf "lines %d-%d" (fst l.first_line) (fst l.last_line)
      in
      (* "characters" represent positions (insertion points) not columns *)
      let pos_s = l.first_byte - snd l.first_line in
      let pos_e = l.last_byte - snd l.last_line + 1 in
      pf ppf "File \"%a\", %a, characters %d-%d"
        pp_path l.file pp_lines l pos_s pos_e

  let pp_gnu ppf l = match l.last_byte < 0 with
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

  let pp_dump ppf l =
    pf ppf "[bytes %d;%d][lines %d;%d][lbytes %d;%d]"
      l.first_byte l.last_byte
      (fst l.first_line) (fst l.last_line)
      (snd l.first_line) (snd l.last_line)

  let pp = pp_gnu

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

module Tdec = struct
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
    { file : Tloc.fpath; i : string; tok : Buffer.t;
      mutable pos : int; mutable line : int; mutable line_pos : int; }

  let from_string ?(file = Tloc.no_file) i =
    { file; i; tok = Buffer.create 255; pos = 0; line = 1; line_pos = 0 }

  (* Location *)

  let file d = d.file
  let pos d = d.pos
  let line d = d.line, d.line_pos

  let loc d ~first_byte ~last_byte ~first_line ~last_line =
    Tloc.v ~file:d.file ~first_byte ~last_byte ~first_line ~last_line

  let loc_to_here d ~first_byte ~first_line =
    let last_line = (d.line, d.line_pos) in
    loc d ~first_byte ~last_byte:d.pos ~first_line ~last_line

  let loc_here d =
    loc_to_here d ~first_byte:d.pos ~first_line:(d.line, d.line_pos)

  (* Errors *)

  exception Err of Tloc.t * string

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
