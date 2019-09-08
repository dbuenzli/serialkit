(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Serialk_text

(* FIXME quickly rehacked after a_meta and l_meta introduction.
   This needs a cleanup. *)

module Sexp = struct

  (* Parse information *)

  type loc = Tloc.t
  let loc_nil = Tloc.nil

  type a_meta =
    { a_loc : loc; a_quoted : string option; a_ws_before : string;
      a_ws_after : string }

  let a_meta_nil =
    { a_loc = loc_nil; a_quoted = None; a_ws_before = ""; a_ws_after = ""; }

  type l_meta =
    { l_loc : loc; l_ws_before : string; l_ws_start : string;
      l_ws_end : string; l_ws_after : string }

  let l_meta_loc l_loc =
    { l_loc; l_ws_before = ""; l_ws_start = ""; l_ws_end = "";
      l_ws_after = ""; }

  let l_meta_nil = l_meta_loc loc_nil

  (* S-expressions *)

  (* FIXME this should become private *)
  type t = [ `A of string * a_meta | `L of t list * l_meta ]
  let loc = function `A (_, p) -> p.a_loc | `L (_, p) -> p.l_loc

  (* Constructors *)

  let atom a = `A (a, a_meta_nil)
  let list l = `L (l, l_meta_nil)

  (* Accessors *)

  let kind = function `A (_, _) -> "atom" | `L (_, _) -> "list"
  let err_exp exp fnd =
    Format.asprintf "%a: %s but expected %s" Tloc.pp (loc fnd) (kind fnd) exp

  let err_exp_atom s = err_exp "atom" s
  let err_exp_list s = err_exp "list" s
  let err_get = invalid_arg

  let to_atom = function `A (a, _) -> Ok a | s -> Error (err_exp_atom s)
  let to_list = function `L (l, _) -> Ok l | s -> Error (err_exp_list s)
  let get_atom = function `A (a, _) -> a | s -> err_get (err_exp_atom s)
  let get_list = function `L (l, _) -> l | s -> err_get (err_exp_list s)

  (* Decode *)

  let curr_char d = (* TODO better escaping *)
    Tdec.tok_reset d; Tdec.tok_accept_uchar d; Tdec.tok_pop d

  let err_eoi msg d ~sbyte ~sline =
    Tdec.err_to_here d ~sbyte ~sline "end of input: %s" msg

  let err_eoi_qtoken = err_eoi "unclosed quoted atom"
  let err_eoi_list = err_eoi "unclosed list"
  let err_eoi_esc = err_eoi "truncated escape"
  let err_illegal_uchar d b = Tdec.err_here d "illegal character U+%04X" b
  let err_rpar d = Tdec.err_here d "mismatched right parenthesis ')'"

  let err_esc_exp_hex d ~sbyte ~sline =
    Tdec.err_to_here d ~sbyte ~sline
      "%s: illegal Unicode escape: expected an hexadecimal digit" (curr_char d)

  let err_esc_uchar d ~sbyte ~sline code =
    Tdec.err_to_here d ~sbyte ~sline
      "illegal Unicode escape: %04X is not a Unicode character" code

  let err_esc_illegal d ~sbyte ~sline pre =
    Tdec.err_to_here d ~sbyte ~sline "%s%s: illegal escape" pre (curr_char d)

  let err_esc_uchar_end d ~sbyte ~sline =
    Tdec.err_to_here d ~sbyte ~sline
      "%s: illegal Unicode escape: expected end of escape '}'"
      (curr_char d)

  let err_esc_char d =
    Tdec.err_here d "escape character '^' illegal outside quoted atoms"

  let dec_byte d = match Tdec.byte d with
  | c when 0x00 <= c && c <= 0x08 || 0x0E <= c && c <= 0x1F || c = 0x7F ->
      err_illegal_uchar d c
  | c -> c
  [@@ ocaml.inline]

  let rec skip_white d = match dec_byte d with
  | 0x20 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D -> Tdec.accept_byte d; skip_white d
  | _ -> ()

  let skip d = (* skip white and comment *)
    let rec skip d = match dec_byte d with
    | 0x20 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D -> Tdec.accept_byte d; skip d
    | 0x3B (* ; *) -> Tdec.accept_byte d; skip_comment d
    | _ -> ()
    and skip_comment d = match dec_byte d with
    | 0x0A | 0x0D -> Tdec.accept_byte d; skip d
    | 0xFFFF -> ()
    | _ -> Tdec.accept_uchar d; skip_comment d
    in
    skip d

  let rec dec_uchar_esc d ~sbyte ~sline =
    match (Tdec.accept_byte d; dec_byte d) with
    | 0x7B (* { *)  ->
        let rec loop d acc count = match (Tdec.accept_byte d; dec_byte d) with
        | c when count > 6 -> err_esc_uchar_end d ~sbyte ~sline
        | c when 0x30 <= c && c <= 0x39 ->
            loop d (acc * 16 + c - 0x30) (count + 1)
        | c when 0x41 <= c && c <= 0x46 ->
            loop d (acc * 16 + c - 0x37) (count + 1)
        | c when 0x62 <= c && c <= 0x66 ->
            loop d (acc * 16 + c - 0x58) (count + 1)
        | 0x7D when count = 0 -> err_esc_exp_hex d ~sbyte ~sline
        | 0x7D when not (Uchar.is_valid acc) ->
            err_esc_uchar d ~sbyte ~sline acc
        | 0x7D ->
            Tdec.accept_byte d; Tdec.tok_add_uchar d (Uchar.unsafe_of_int acc);
        | 0xFFFF -> err_eoi_esc d ~sbyte ~sline
        | _ -> err_esc_exp_hex d ~sbyte ~sline
        in
        loop d 0 0
    | 0xFFFF -> err_eoi_esc d ~sbyte ~sline
    | c -> err_esc_illegal d ~sbyte ~sline "^u"

  let rec dec_esc d =
    let sbyte = Tdec.pos d and sline = Tdec.line d in
    match (Tdec.accept_byte d; dec_byte d) with
    | 0x22 -> Tdec.accept_byte d; Tdec.tok_add_char d '"'
    | 0x5E -> Tdec.accept_byte d; Tdec.tok_add_char d '^'
    | 0x6E -> Tdec.accept_byte d; Tdec.tok_add_char d '\n'
    | 0x72 -> Tdec.accept_byte d; Tdec.tok_add_char d '\r'
    | 0x20 -> Tdec.accept_byte d; Tdec.tok_add_char d ' '
    | 0x75 -> dec_uchar_esc d ~sbyte ~sline
    | 0x0A | 0x0D -> (* continuation line *) skip_white d
    | 0xFFFF -> err_eoi_esc d ~sbyte ~sline
    | _ -> err_esc_illegal d ~sbyte ~sline "^"

  let rec dec_qtoken d =
    let sbyte = Tdec.pos d and sline = Tdec.line d in
    let rec loop d = match dec_byte d with
    | 0x22 ->
        let a = Tdec.tok_pop d in
        let a_quoted =
          (* TODO this should preserve escapes. It seems we are better
             off to simply tokenize without escaping and then parse the
             tok. But problem for errr report add an alternate raw token to
             decoder ? *)
          Some a
        in
        let a_loc = Tdec.loc_to_here d ~sbyte ~sline in
        let m = { a_loc; a_quoted; a_ws_before = ""; a_ws_after = "" } in
        Tdec.accept_byte d; `A (a, m)
    | 0x5E -> dec_esc d; loop d
    | 0xFFFF -> err_eoi_qtoken d ~sbyte ~sline
    | _ -> Tdec.tok_accept_uchar d; loop d
    in
    Tdec.accept_byte d; loop d

  and dec_token d =
    let sbyte = Tdec.pos d and sline = Tdec.line d in
    let rec loop d = match dec_byte d with
    | 0x28 | 0x29 | 0x3B | 0x22
    | 0x20 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D
    | 0xFFFF ->
        let ebyte = Tdec.pos d - 1 in
        let eline = Tdec.line d in
        let a_loc = Tdec.loc d ~sbyte ~ebyte ~sline ~eline in
        let m = { a_loc; a_quoted = None; a_ws_before = ""; a_ws_after = "" } in
        `A (Tdec.tok_pop d, m)
    | 0x5E -> err_esc_char d
    | _ -> Tdec.tok_accept_uchar d; loop d
    in
    loop d

  and dec_eoi d stack acc = match stack with
  | (sbyte, sline, _) :: [] ->
      begin match acc with
      | [] ->
          let l_loc = Tdec.loc d ~sbyte:0 ~ebyte:0 ~sline:(1,0) ~eline:(1,0) in
          let m = { l_loc; l_ws_before = ""; l_ws_start = ""; l_ws_end = "";
                    l_ws_after = "" } in
          `L ([], m)
      | acc ->
          let eloc = loc (List.hd acc) in
          let acc = List.rev acc in
          let sloc = loc (List.hd acc) in
          let l_loc = Tloc.merge sloc eloc  in
          let m = { l_loc; l_ws_before = ""; l_ws_start = ""; l_ws_end = "";
                    l_ws_after = "" }
          in
          `L (acc, m)
      end
  | (sbyte, sline, _) :: locs -> err_eoi_list d ~sbyte ~sline
  | [] -> assert false

  and dec_sexp_seq d stack acc = match (skip d; dec_byte d) with
  | 0x28 ->
      let stack = (Tdec.pos d, Tdec.line d, acc) :: stack in
      Tdec.accept_byte d; dec_sexp_seq d stack []
  | 0x29 ->
      begin match stack with
      | (sbyte, sline, _) :: [] -> err_rpar d
      | (sbyte, sline, prev_acc) :: stack ->
          let ebyte = Tdec.pos d and eline = Tdec.line d in
          let l_loc = Tdec.loc d ~sbyte ~ebyte ~sline ~eline in
          let m = { l_loc; l_ws_before = ""; l_ws_start = ""; l_ws_end = "";
                    l_ws_after = "" }
          in
          let acc = `L (List.rev acc, m) :: prev_acc in
          Tdec.accept_byte d; dec_sexp_seq d stack acc
      | [] -> assert false
      end
  | 0xFFFF -> dec_eoi d stack acc
  | 0x22 -> dec_sexp_seq d stack (dec_qtoken d :: acc)
  | _ -> dec_sexp_seq d stack (dec_token d :: acc)

  let seq_of_string ?(file = Tloc.no_file) s =
    try
      let d = Tdec.create ~file s in
      Ok (dec_sexp_seq d [(0, (1, 0), [])] [])
    with Tdec.Err (loc, msg) ->
      Error (Format.asprintf "@[<v>%a:@,%s@]" Tloc.pp loc msg)

  (* s-expression generation. *)

  let must_quote a =
    let rec loop max i s = match i < max with
    | false -> if max < 0 then true (* empty string *) else false
    | true ->
        match a.[i] with
        | '\x00' .. '\x1F' | '\x7F' | ' ' (* ctrl + white *)
        | '(' | ')' | ';' | '"' (* atom separators *)
        | '^'  (* escape char *) -> true
        | c -> loop max (i + 1) s
    in
    loop (String.length a - 1) 0 a

  let buffer_add_qatom b a = (* Adds a quoted atom to [b] *)
    let len = String.length a in
    let flush start i =
      if start < len then Buffer.add_substring b a start (i - start)
    in
    let rec loop start i = match i < len with
    | false -> flush start i
    | true ->
        let next = i + 1 in
        match a.[i] with
        | '"' -> flush start i; Buffer.add_string b "^\""; loop next next
        | '^' -> flush start i; Buffer.add_string b "^^"; loop next next
        | '\n' -> flush start i; Buffer.add_string b "^n"; loop next next
        | '\r' -> flush start i; Buffer.add_string b "^r"; loop next next
        | '\x00' .. '\x1F' | '\x7F' as c (* ctrl + white except ' ' *) ->
            flush start i;
            Buffer.add_string b (Format.asprintf "^u{%04X}" (Char.code c));
            loop next next
        | c -> loop start next
    in
    Buffer.add_char b '\"'; loop 0 0; Buffer.add_char b '\"'

  let buffer_add_atom b a = match must_quote a with
  | false -> Buffer.add_string b a
  | true -> buffer_add_qatom b a

  module G = struct
    (* Not T.R. we could CPS. *)

    type enc = { mutable sep : bool; b : Buffer.t }
    type t = enc -> unit
    let addc c enc = Buffer.add_char enc.b c [@@ ocaml.inline]
    let adds s enc = Buffer.add_string enc.b s [@@ ocaml.inline]
    let adds_atom a enc = buffer_add_atom enc.b a

    let nosep enc = enc.sep <- false
    let sep enc = enc.sep
    let set_sep sep enc = enc.sep <- sep
    let if_sep enc = if not enc.sep then enc.sep <- true else addc ' ' enc

    (* Generation *)

    type lyst = t

    let atom = adds_atom
    let ls enc = ()
    let le els enc =
      let sep = sep enc in
      addc '(' enc; nosep enc; els enc; addc ')' enc; set_sep sep enc

    let el e l enc = l enc; if_sep enc; e enc
    let el_if c e l enc = if c then el (e ()) l enc else l enc

    (* Derived generators. *)

    let atomf fmt = Format.kasprintf atom fmt
    let list elv data = le (List.fold_left (fun l v -> el (elv v) l) ls data)

    let bool b = adds (string_of_bool b)
    let int i  = adds (string_of_int i)
    let float f = adds (Format.sprintf "%g" f)
    let float_hex f = adds (Format.sprintf "%h" f)
    let string = atom

    let option some o enc = match o with
    | None -> adds "none" enc
    | Some v -> le (el (some v) (el (adds "some") ls)) enc

    let rec sexp = function (* not T.R. *)
    | `A (a, _) -> atom a
    | `L (l, _) -> le @@ List.fold_left (fun l v -> el (sexp v) l) ls l

    (* Output *)

    let enc b = { sep = true; b }
    let buffer_add b g = g (enc b)
    let to_string g =
      let b = Buffer.create 65535 in
      (buffer_add b g; Buffer.contents b)
  end

  let seq_to_string s =
    let g s enc = match s with
    | `A (a, _) -> G.adds_atom a enc
    | `L (data, _) ->
        G.nosep enc;
        (List.fold_left (fun l v -> G.el (G.sexp v) l) G.ls data) enc
    in
    G.to_string (g s)

  let quote b a = match must_quote a with
  | false -> a
  | true ->
      buffer_add_qatom b a;
      let a = Buffer.contents b in
      Buffer.reset b; a

  let _pp ~layout b ppf s = match layout with (* XXX cleanup *)
  | false ->
      let rec loop ~sp = function
      | ((`A (a, _) :: ss) :: todo) ->
          if sp then Format.pp_print_space ppf ();
          Format.pp_print_string ppf (quote b a); loop ~sp:true (ss :: todo)
      | ((`L (l, _) :: ss) :: todo) ->
          if sp then Format.pp_print_space ppf ();
          Format.fprintf ppf "@[<1>("; loop ~sp:false (l :: ss :: todo)
      | ([] :: []) -> ()
      | ([] :: todo) -> Format.fprintf ppf ")@]"; loop ~sp:true todo
      | [] -> assert false
      in
      loop ~sp:false [[s]]
  | true ->
      (* FIXME do the layout entirely by hand to avoid Format suprises. *)
      let rec loop ~sp = function
      | ((`A (a, m) :: ss), me) :: todo ->
          (if m.a_ws_before = ""
           then (if sp then Format.pp_print_char ppf ' ')
           else Format.pp_print_string ppf m.a_ws_before);
          Format.pp_print_string ppf (quote b a);
          Format.pp_print_string ppf m.a_ws_after;
          loop ~sp:(m.a_ws_after = "") ((ss, me) :: todo)
      | ((`L (l, m) :: ss), me) :: todo ->
          (if m.l_ws_before = ""
           then (if sp then Format.pp_print_char ppf ' ')
           else Format.pp_print_string ppf m.l_ws_before);
          Format.pp_print_char ppf '(';
          Format.pp_print_string ppf m.l_ws_start;
          loop ~sp:false ((l, m) :: (ss, me) :: todo)
      | ([], _) :: [] -> ()
      | ([], me) :: todo ->
          Format.pp_print_string ppf me.l_ws_end;
          Format.pp_print_char ppf ')';
          Format.pp_print_string ppf me.l_ws_after;
          loop ~sp:(me.l_ws_after = "") todo
      | [] -> assert false
      in
      loop ~sp:false [([s], l_meta_nil)]

  let pp ppf s = _pp ~layout:false (Buffer.create 255) ppf s
  let pp_layout ppf s = _pp ~layout:true (Buffer.create 255) ppf s

  let _pp_seq ~layout ppf = function
  | `A _ as s -> pp ppf s
  | `L (l, m)->
      let pp = _pp ~layout (Buffer.create 255) in
      match layout with
      | true ->
          Format.fprintf ppf "@[<v>%a@]" (Format.pp_print_list pp) l
      | false ->
          Format.fprintf ppf "@[<v>%s%s%a%s%s@]"
            m.l_ws_before m.l_ws_start
            (Format.pp_print_list pp) l
            m.l_ws_end m.l_ws_after

  let pp_seq ppf s = _pp_seq ~layout:false ppf s
  let pp_seq_layout ppf s = _pp_seq ~layout:true ppf s

  let rec pp_dump ppf = function (* Not T.R *)
  | `A (a, m) -> Format.fprintf ppf "(A:%a %S)" Tloc.pp_dump m.a_loc  a
  | `L (vs, m) ->
      Format.fprintf ppf "@[<1>(L:%a " Tloc.pp_dump m.l_loc;
      Format.pp_print_list pp_dump ppf vs;
      Format.fprintf ppf ")@]"

  (* Paths *)

  type index = Key of string | Nth of int

  let rec pave_stubs ?(stub = atom "") n l =
    if n <= 0 then l else pave_stubs (n - 1) (stub :: l)

  let pave_index ?stub i s = match i with
  | Key k -> list [atom k; s]
  | Nth n when n < 0 -> list (s :: pave_stubs ?stub (-n - 1) [])
  | Nth n -> list (pave_stubs n [s])

  let seq_pave_index ?stub i = function
  | `A _ as s -> pave_index ?stub i s
  | `L (l, _) ->
      match i with
      | Key k -> list (atom k :: l)
      | Nth n when n < 0 ->
          let n = (-n - 1) - (List.length l - 1) in
          list (List.rev (pave_stubs ?stub n (List.rev l)))
      | Nth n -> list (pave_stubs n l)

  let pp_index ppf = function
  | Key k -> Format.pp_print_string ppf k
  | Nth n -> Format.fprintf ppf "[%d]" n

  let pp_bracketed_index ppf = function
  | Key k -> Format.fprintf ppf "[%s]" k
  | Nth n -> Format.fprintf ppf "[%d]" n

  type path = index list (* reversed *)
  let path_err i fmt = Format.kasprintf failwith ("%d: " ^^ fmt) i
  let path_err_unexp_eoi i = path_err i "unexpected end of input"
  let path_err_unexp_char i s = path_err i "unexpected character: %C" s.[i]
  let path_err_illegal_char i s = path_err i "illegal character here: %C" s.[i]
  let err_unexp i s =
    path_err i "unexpected input: %S" (Tloc.string_with_index_range ~first:i s)

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
    let idx = Tloc.string_with_index_range ~first ~last s in
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

  let pp_path ppf is =
    let pp_sep ppf () = Format.pp_print_char ppf '.' in
    Format.pp_print_list ~pp_sep pp_index ppf (List.rev is)

  let pave_path ?stub p s =
    let rec loop prev_nth s = function
    | (Nth _ as i) :: is -> loop true (pave_index ?stub i s) is
    | (Key k) :: is when prev_nth -> loop false (list (atom k :: get_list s)) is
    | (Key k) :: is -> loop false (list [atom k; s]) is
    | [] -> s
    in
    loop false s p

  let seq_pave_path ?stub p s = match p with
  | [] -> s
  | i :: is -> pave_path ?stub is (seq_pave_index ?stub i s)

  (* Carets *)

  type caret =
  | Before of path
  | Over of path
  | After of path

  let caret_path = function Before p | After p | Over p -> p
  let repath_caret p = function
  | Before _ -> Before p | Over _ -> Over p | After _ -> After p

  let caret_of_string s =
    let rec loop p s i max =
      if i > max then Over p else
      let next = i + 1 in
      match s.[i] with
      | 'v' when next <= max && s.[next] = '[' ->
          let next, p = path_parse_index p s next max in
          path_parse_eoi s next max; Before p
      | c ->
          let next, p = path_parse_index p s i max in
          if next > max then Over p else
          if s.[next] = 'v'
          then (path_parse_eoi s (next + 1) max; After p) else
          if s.[next] <> '.' then path_err_unexp_char next s else
          if next + 1 <= max then loop p s (next + 1) max else
          path_err_unexp_eoi next
    in
    try
      if s = "" then Ok (Over []) else
      let start = if s.[0] = '.' then 1 else 0 in
      Ok (loop [] s start (String.length s - 1))
    with Failure e -> Error e

  let pp_caret ppf = function
  | Over p -> pp_path ppf p
  | Before (c :: p) ->
      pp_path ppf p; (if p <> [] then Format.pp_print_char ppf '.');
      Format.pp_print_char ppf 'v'; pp_bracketed_index ppf c
  | After (c :: p) ->
      pp_path ppf p; (if p <> [] then Format.pp_print_char ppf '.');
      pp_bracketed_index ppf c; Format.pp_print_char ppf 'v'
  | _ -> ()

  let _pave_caret paver ?stub c s = match c with
  | Over p -> paver ?stub p s
  | Before [] | After [] -> list [s]
  | Before (_ :: is) -> paver ?stub is (list [s])
  | After (_ :: is) -> paver ?stub is (list [s])

  let pave_caret = _pave_caret pave_path
  let seq_pave_caret = _pave_caret seq_pave_path
end

module Sexpg = Sexp.G
module Sexpq = struct
  module Sset = Set.Make (String)
  module Smap = Map.Make (String)
  type path = (Sexp.index * Sexp.loc) list (* reversed *)

  let pp_quote ppf s = Format.fprintf ppf "'%s'" s
  let pp_key = pp_quote
  let pp_path ?(pp_key = pp_key) ppf p =
    let pp_index pp_key ppf = function
    | Sexp.Key k, l -> Format.fprintf ppf "%a: in key %a" Tloc.pp l pp_key k
    | Sexp.Nth i, l ->
        Format.fprintf ppf "%a: in element %d of sequence" Tloc.pp l i
    in
    Format.fprintf ppf "@[<v>%a@]" (Format.pp_print_list (pp_index pp_key)) p

  let path_to_string p = Format.asprintf "%a" (pp_path ?pp_key:None) p

  (* Errors *)

  exception Err of path * Tloc.t * string

  let err p l msg = raise_notrace (Err (p, l, msg))
  let errf p l fmt = Format.kasprintf (err p l) fmt
  let err_exp_fnd_raw exp p l fnd =
    errf p l "found %s but expected %s" fnd exp

  let err_exp exp p fnd =
    errf p (Sexp.loc fnd) "found %s but expected %s" (Sexp.kind fnd) exp

  let err_exp_atom = err_exp "atom"
  let err_exp_list = err_exp "list"
  let err_empty_list p l = errf p l "unexpected empty list"
  let err_to_string p loc msg =
    let pp_lines ppf s =
      Format.fprintf ppf "@[<v>%a@]"
        (Format.pp_print_list Format.pp_print_string)
        (String.split_on_char '\n' s)
    in
    match p with
    | [] -> Format.asprintf "%a:@\n%a" Tloc.pp loc pp_lines msg
    | p ->
        Format.asprintf "%a:@\n%a@\n  @[%a@]"
          Tloc.pp loc pp_lines msg (pp_path ?pp_key:None) p

  let esc_atom a = a (* TODO (for error report) *)

  (* Queries *)

  type 'a t = path -> Sexp.t -> 'a

  let query q s = try Ok (q [] s) with
  | Err (p, l, m) -> Error (err_to_string p l m)

  let query' q s = try Ok (q [] s) with
  | Err (p, l, m) -> Error (p, l, m)

  (* Succeeding and failing queries *)

  let succeed v p s = v
  let fail msg p s = err p (Sexp.loc s) msg
  let failf fmt = Format.kasprintf fail fmt

  (* Query combinators *)

  let app fq q p s = fq p s (q p s)
  let ( $ ) = app
  let bind q f p s = f (q p s) p s
  let map f q p s = f (q p s)
  let some q p s = Some (q p s)

  (* S-expression queries *)

  let fold ~atom ~list p = function
  | `A _ as s -> atom p s
  | `L _ as s -> list p s

  let sexp p s = s
  let loc p s = Sexp.loc s
  let with_loc q p s = (q p s), Sexp.loc s

  (* Atom queries *)

  let atom p = function `A (a, _) -> a | `L (_, _) as s  -> err_exp_atom p s
  let parsed_atom ~kind parse p = function
  | `A (a, _) as s -> (match parse a with Ok v -> v | Error m -> fail m p s)
  | `L (_, _) as s -> err_exp kind p s

  let enum ~kind ss p = function
  | `A (a, _) when Sset.mem a ss -> a
  | `A (a, m) ->
      let ss = Sset.elements ss in
      let did_you_mean = Tdec.err_did_you_mean ~kind pp_quote in
      let suggestions = match Tdec.err_suggest ss a with
      | [] -> ss | ss -> ss
      in
      errf p m.Sexp.a_loc "%a" did_you_mean (a, suggestions)
  | `L (_, _) as s -> err_exp kind p s

  let enum_map ~kind sm p = function
  | `L (_, _) as s -> err_exp kind p s
  | `A (a, m) ->
      match Smap.find a sm with
      | v -> v
      | exception Not_found ->
          let ss = Smap.fold (fun k _ acc -> k :: acc) sm [] in
          let did_you_mean = Tdec.err_did_you_mean ~kind pp_quote in
          let suggs = match Tdec.err_suggest ss a with [] -> ss | ss -> ss in
          errf p m.Sexp.a_loc "%a" did_you_mean (a, suggs)

  let parsed_exn_atom ~kind parse p = function
  | `L (_, _) as s -> err_exp kind p s
  | `A (a, m) ->
      try parse a with
      | Failure _ ->
          errf p m.Sexp.a_loc "%s: could not parse %s" (esc_atom a) kind

  let _tf = "true or false"
  let bool p = function
  | `A ("true", _) -> true
  | `A ("false", _) -> false
  | `A (a, m) -> err_exp_fnd_raw _tf p m.Sexp.a_loc (esc_atom a)
  | `L (_, _) as s -> err_exp _tf p s

  let int = parsed_exn_atom ~kind:"integer" int_of_string
  let int32 = parsed_exn_atom ~kind:"int32" Int32.of_string
  let int64 = parsed_exn_atom ~kind:"int64" Int64.of_string
  let float = parsed_exn_atom ~kind:"float" float_of_string

  (* List queries *)

  let is_empty p = function
  | `L (vs, _) -> vs = []
  | `A (_, _) as s -> err_exp_list p s

  let hd q p = function
  | `L (v :: _, m) -> q ((Sexp.Nth 0, m.Sexp.l_loc) :: p) v
  | `L ([], m) -> err_empty_list p m.Sexp.l_loc
  | `A (_, _) as s -> err_exp_list p s

  let tl q p = function
  | `L (_ :: [], m) ->
      q p (`L ([], Sexp.l_meta_loc m.Sexp.l_loc))
  | `L (_ :: (v :: _ as s), m) ->
      let l_loc = Tloc.restart ~at:(Tloc.to_start (Sexp.loc v)) m.Sexp.l_loc in
      q p (`L (s, Sexp.l_meta_loc l_loc))
  | `L ([], m) -> err_empty_list p m.Sexp.l_loc
  | `A (_, _) as s -> err_exp_list p s

  let list_find n p = function
  | `A (_, _) as s -> err_exp_list p s
  | `L (vs, m) ->
      let k, vs = if n < 0 then -n - 1, List.rev vs else n, vs in
      match List.nth vs k with
      | v -> Ok v | exception Failure _ -> Error (vs, m.Sexp.l_loc)

  let nth n q p s = match list_find n p s with
  | Ok v -> q ((Sexp.Nth n, Sexp.loc v) :: p) v
  | Error (vs, l) -> errf p l "%d: no such index in list" n

  let find_nth n q ~absent p s = match list_find n p s with
  | Ok v -> q ((Sexp.Nth n, Sexp.loc v) :: p) v
  | Error (_, _) -> absent

  let fold_list f q acc p = function
  | `A (_, _) as s -> err_exp_list p s
  | `L (vs, m) ->
      let rec loop f q acc i l p = function
      | [] -> acc
      | v :: vs ->
          let acc = f (q ((Sexp.Nth i, l) :: p) v) acc in
          loop f q acc (i + 1) l p vs
      in
      loop f q acc 0 m.Sexp.l_loc p vs

  let list q = map List.rev (fold_list List.cons q [])

  let lone_atom q p = function
  | `A (_, _)  as a -> q p a
  | `L ([`A _ as a], _) -> q p a
  | `L ([], m) -> err_exp_fnd_raw "atom" p m.Sexp.l_loc "nothing"
  | `L (_, m) -> err_exp_fnd_raw "an atom" p m.Sexp.l_loc "list"

  (* Dictionaries *)

  let err_exp_dict = err_exp "dictionary"

  let dict_dom bs =
    let rec loop dom = function
    | `L (`A (a, _) :: _, _) :: vs -> loop (Sset.add a dom) vs
    | _ :: bs -> loop dom bs
    | [] -> dom
    in
    loop Sset.empty bs

  let dict_find k p = function
  | `A (_, _) as s -> err_exp_dict p s
  | `L (bs, m) ->
      let rec loop res = function
      | `L (`A (a, _) :: vs, m) :: bs when String.equal a k ->
          let l_loc = match vs with
          | [] ->(* XXX problem how to span emptyness... *)
              Tloc.to_end m.Sexp.l_loc
          | vs ->
                Tloc.merge (Sexp.loc (List.hd vs)) (Sexp.loc List.(hd (rev vs)))
          in
          let m' = Sexp.l_meta_loc l_loc in
          loop (Ok (m, `L (vs, m'))) bs (* last one takes over so we cont. *)
      | [] -> res
      | _ :: bs -> loop res bs
      in
      loop (Error (bs, m.Sexp.l_loc)) bs

  let key k q p s = match dict_find k p s with
  | Ok (bmeta, v) -> q ((Sexp.Key k, bmeta.Sexp.l_loc) :: p) v
  | Error (bs, dloc) ->
      let dom = dict_dom bs in
      let keys = Sset.elements dom in
      let pre ppf () = Format.pp_print_string ppf "Unbound" in
      let did_you_mean = Tdec.err_did_you_mean ~pre ~kind:"key" pp_key in
      errf p dloc "%a" did_you_mean (k, Tdec.err_suggest keys k)

  let find_key k q ~absent p s = match dict_find k p s with
  | Ok (bmeta, v) -> q ((Sexp.Key k, bmeta.Sexp.l_loc) :: p) v
  | Error (_, _) -> absent

  let key_dom ~validate p = function
  | `A (_, _) as s -> err_exp_dict p s
  | `L (bs, _) ->
      let add_key = match validate with
      | None -> fun p m k acc -> Sset.add k acc
      | Some dom ->
          fun p m k acc -> match Sset.mem k dom with
          | true -> Sset.add k acc
          | false ->
              let keys = Sset.elements dom in
              let did_you_mean = Tdec.err_did_you_mean ~kind:"key" pp_key in
              errf p m.Sexp.a_loc "%a" did_you_mean (k, Tdec.err_suggest keys k)
      in
      let add_key validate acc = function
      | `L (`A (k, m) :: v, _) -> add_key p m k acc
      | `L ([], m) ->
          err_exp_fnd_raw "(atom ...) list" p m.Sexp.l_loc "empty list"
      | `L (_, m) ->
          err_exp_fnd_raw "(atom ...) list" p m.Sexp.l_loc "malformed list"
      | `A (_, _) as s -> err_exp_list p s
      in
      List.fold_left (add_key validate) Sset.empty bs

  (* Path queries *)

  let index i q = match i with Sexp.Key k -> key k q | Sexp.Nth n -> nth n q

  let find_index i q ~absent = match i with
  | Sexp.Key k -> find_key k q ~absent
  | Sexp.Nth n -> find_nth n q ~absent

  let path p q = List.fold_left (fun acc i -> index i acc) q p
  let find_path p q ~absent =
    List.fold_left (fun acc i -> find_index i acc ~absent) q p

  let probe_path is p s =
    let rec loop p s = function
    | [] -> Sexp.loc s, []
    | Sexp.Key k :: is as missing ->
        begin match dict_find k p s with
        | Ok (bmeta, v) -> loop ((Sexp.Key k, bmeta.Sexp.l_loc) :: p) v is
        | Error (bs, dloc) -> dloc, List.rev missing
        end
    | Sexp.Nth i :: is as missing ->
        begin match list_find i p s with
        | Ok v -> loop ((Sexp.Nth i, Sexp.loc v) :: p) v is
        | Error (_, lloc) -> lloc, List.rev missing
        end
    in
    loop p s (List.rev is)

  (* OCaml encoding queries *)

  let option q p = function (* TODO improve *)
  | `A ("none", m) -> None
  | `L ((`A ("some", _) :: v), m) ->
      Some (q ((Sexp.Key "some", m.Sexp.l_loc) (* ? *) :: p) (`L (v, m)))
  | `A (a, m) ->
      err_exp_fnd_raw "none or (some ...)" p m.Sexp.a_loc (esc_atom a)
  | `L ((`A (a, _) :: v), m) ->
      err_exp_fnd_raw "none or (some ...)" p m.Sexp.l_loc ("(" ^ (esc_atom a))
  | `L (_, m) ->
      err_exp_fnd_raw "none or (some ...)" p m.Sexp.l_loc "an arbitrary list"
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
