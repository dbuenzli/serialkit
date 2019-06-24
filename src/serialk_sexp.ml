(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let string_with_index_range ?(first = 0) ?last s =
  let max = String.length s - 1 in
  let last = match last with
  | None -> max
  | Some l when l > max -> max
  | Some l -> l
  in
  let first = if first < 0 then 0 else first in
  if first > last then "" else
  String.sub s first (last - first + 1)

module Sexp = struct
  open Serialk_tlex

  (* S-expressions *)

  type loc = Tloc.t
  type t = [ `A of string * loc | `L of t list * loc ]

  let loc_nil = Tloc.nil
  let loc = function `A (_, loc) | `L (_, loc) -> loc

  (* Constructors *)

  let atom a = `A (a, loc_nil)
  let list l = `L (l, loc_nil)

  (* Accessors *)

  let kind = function `A _ -> "atom" | `L _ -> "list"
  let err_exp exp fnd =
    Format.asprintf "%a: %s but expected %s" Tloc.pp (loc fnd) (kind fnd) exp

  let err_exp_atom = err_exp "atom"
  let err_exp_list = err_exp "list"

  let err e = Error e
  let to_atom = function `A (a, _) -> Ok a | s -> err (err_exp_atom s)
  let to_list = function `L (l, _) -> Ok l | s -> err (err_exp_list s)

  let err = invalid_arg
  let get_atom = function `A (a, _) -> a | s -> err (err_exp_atom s)
  let get_list = function `L (l, _) -> l | s -> err (err_exp_list s)

  (* Decode *)

  let err_char d = (* TODO better escaping *)
    Tdec.tok_reset d; Tdec.tok_accept_uchar d; Tdec.tok_pop d

  let err_eoi msg d ~byte_s ~line_s =
    Tdec.err_to_here d ~byte_s ~line_s "end of input: %s" msg

  let err_eoi_qtoken = err_eoi "unclosed quoted atom"
  let err_eoi_list = err_eoi "unclosed list"
  let err_eoi_esc = err_eoi "truncated escape"
  let err_illegal_uchar d b = Tdec.err_here d "illegal character U+%04X" b
  let err_rpar d = Tdec.err_here d "mismatched right parenthesis ')'"

  let err_esc_exp_hex d ~byte_s ~line_s =
    Tdec.err_to_here d ~byte_s ~line_s
      "%s: illegal Unicode escape: expected an hexadecimal digit" (err_char d)

  let err_esc_uchar d ~byte_s ~line_s code =
    Tdec.err_to_here d ~byte_s ~line_s
      "illegal Unicode escape: %04X is not a Unicode character" code

  let err_esc_illegal d ~byte_s ~line_s pre =
    Tdec.err_to_here d ~byte_s ~line_s "%s%s: illegal escape" pre (err_char d)

  let err_esc_uchar_end d ~byte_s ~line_s =
    Tdec.err_to_here d ~byte_s ~line_s
      "%s: illegal Unicode escape: expected end of escape '}'"
      (err_char d)

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

  let rec dec_uchar_esc d ~byte_s ~line_s =
    match (Tdec.accept_byte d; dec_byte d) with
    | 0x7B (* { *)  ->
        let rec loop d acc count = match (Tdec.accept_byte d; dec_byte d) with
        | c when count > 6 -> err_esc_uchar_end d ~byte_s ~line_s
        | c when 0x30 <= c && c <= 0x39 ->
            loop d (acc * 16 + c - 0x30) (count + 1)
        | c when 0x41 <= c && c <= 0x46 ->
            loop d (acc * 16 + c - 0x37) (count + 1)
        | c when 0x62 <= c && c <= 0x66 ->
            loop d (acc * 16 + c - 0x58) (count + 1)
        | 0x7D when count = 0 -> err_esc_exp_hex d ~byte_s ~line_s
        | 0x7D when not (Uchar.is_valid acc) ->
            err_esc_uchar d ~byte_s ~line_s acc
        | 0x7D ->
            Tdec.accept_byte d; Tdec.tok_add_uchar d (Uchar.unsafe_of_int acc);
        | 0xFFFF -> err_eoi_esc d ~byte_s ~line_s
        | _ -> err_esc_exp_hex d ~byte_s ~line_s
        in
        loop d 0 0
    | 0xFFFF -> err_eoi_esc d ~byte_s ~line_s
    | c -> err_esc_illegal d ~byte_s ~line_s "^u"

  let rec dec_esc d =
    let byte_s = Tdec.pos d and line_s = Tdec.line d in
    match (Tdec.accept_byte d; dec_byte d) with
    | 0x22 -> Tdec.accept_byte d; Tdec.tok_add_char d '"'
    | 0x5E -> Tdec.accept_byte d; Tdec.tok_add_char d '^'
    | 0x6E -> Tdec.accept_byte d; Tdec.tok_add_char d '\n'
    | 0x72 -> Tdec.accept_byte d; Tdec.tok_add_char d '\r'
    | 0x20 -> Tdec.accept_byte d; Tdec.tok_add_char d ' '
    | 0x75 -> dec_uchar_esc d ~byte_s ~line_s
    | 0x0A | 0x0D -> (* continuation line *) skip_white d
    | 0xFFFF -> err_eoi_esc d ~byte_s ~line_s
    | _ -> err_esc_illegal d ~byte_s ~line_s "^"

  let rec dec_qtoken d =
    let byte_s = Tdec.pos d and line_s = Tdec.line d in
    let rec loop d = match dec_byte d with
    | 0x22 ->
        let loc = Tdec.loc_to_here d ~byte_s ~line_s in
        Tdec.accept_byte d; `A (Tdec.tok_pop d, loc)
    | 0x5E -> dec_esc d; loop d
    | 0xFFFF -> err_eoi_qtoken d ~byte_s ~line_s
    | _ -> Tdec.tok_accept_uchar d; loop d
    in
    Tdec.accept_byte d; loop d

  and dec_token d =
    let byte_s = Tdec.pos d and line_s = Tdec.line d in
    let rec loop d = match dec_byte d with
    | 0x28 | 0x29 | 0x3B | 0x22
    | 0x20 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D
    | 0xFFFF as c ->
        let byte_e = Tdec.pos d - 1 in
        let line_e = match c with
        | 0x0A | 0x0D ->
            let (pos, line) = Tdec.line d in
            (pos - 1, line - 1)
        | _ -> Tdec.line d
        in
        let loc = Tloc.v ~file:(Tdec.file d) ~byte_s ~byte_e ~line_s ~line_e in
        `A (Tdec.tok_pop d, loc)
    | 0x5E -> err_esc_char d
    | _ -> Tdec.tok_accept_uchar d; loop d
    in
    loop d

  and dec_eoi d stack acc = match stack with
  | (byte_s, line_s, _) :: [] ->
      let byte_e = Tdec.pos d - 1 and line_e = Tdec.line d in
      let loc = Tdec.loc d ~byte_s ~byte_e ~line_s ~line_e in
      `L (List.rev acc, loc)
  | (byte_s, line_s, _) :: locs -> err_eoi_list d ~byte_s ~line_s
  | [] -> assert false

  and dec_sexp_seq d stack acc = match (skip d; dec_byte d) with
  | 0x28 ->
      let stack = (Tdec.pos d, Tdec.line d, acc) :: stack in
      Tdec.accept_byte d; dec_sexp_seq d stack []
  | 0x29 ->
      begin match stack with
      | (byte_s, line_s, _) :: [] -> err_rpar d
      | (byte_s, line_s, prev_acc) :: stack ->
          let byte_e = Tdec.pos d and line_e = Tdec.line d in
          let loc = Tdec.loc d ~byte_s ~byte_e ~line_s ~line_e in
          let acc = `L (List.rev acc, loc) :: prev_acc in
          Tdec.accept_byte d; dec_sexp_seq d stack acc
      | [] -> assert false
      end
  | 0xFFFF -> dec_eoi d stack acc
  | 0x22 -> dec_sexp_seq d stack (dec_qtoken d :: acc)
  | _ -> dec_sexp_seq d stack (dec_token d :: acc)

  let of_string ?(file = Tloc.no_file) s =
    try
      let d = Tdec.create ~file s in
      Ok (dec_sexp_seq d [(0, (0, 1), [])] [])
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

  let to_string s =
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

  let _pp b ppf s =
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

  let pp ppf s = _pp (Buffer.create 255) ppf s

  let pp_seq ppf = function
  | `A _ as s -> pp ppf s
  | `L (l, _) ->
      let pp = _pp (Buffer.create 255) in
      Format.fprintf ppf "@[<v>%a@]" (Format.pp_print_list pp) l
end

module Sexpg = Sexp.G
module Sexpq = struct
  open Serialk_tlex

  module Sset = Set.Make (String)
  module Smap = Map.Make (String)

  let pp_quote ppf s = Format.fprintf ppf "'%s'" s
  let pp_key = pp_quote
  (* Fmt.tty [`Fg `Yellow; `Bold] (Fmt.quote ~mark:"'" Fmt.string) *)

  type path = (* Paths in s-expressions lists and dictionaries traversals *)
    ([`L | `K of string ] * Sexp.loc) list (* in reverse order *)

  let path_to_string p =
    let seg = function `L, _ -> "()" | `K n, _ -> "." ^ n in
    String.concat "" (List.rev_map seg p)

  let path_to_trace ?(pp_key = pp_key) p =
    let seg = function
    | `L, l -> Format.asprintf "%a: in list" Tloc.pp l
    | `K k, l -> Format.asprintf "%a: in key %a" Tloc.pp l pp_key k
    in
    String.concat "\n" (List.map seg p)

  (* Errors *)

  exception Err of path * Tloc.t * string

  let err p l msg = raise_notrace (Err (p, l, msg))
  let errf p l fmt = Format.kasprintf (err p l) fmt
  let err_exp_fnd_raw exp p l fnd = errf p l "found %s but expected %s" fnd exp
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
          Tloc.pp loc pp_lines msg pp_lines (path_to_trace p)

  let esc_atom a = a (* TODO *)

  (* Queries *)

  type 'a t = path -> Sexp.t -> 'a

  let query q s = try Ok (q [] s) with
  | Err (p, l, m) -> Error (err_to_string p l m)

  (* Succeeding and failing queries *)

  let succeed v p s = v
  let fail msg p s = err p (Sexp.loc s) msg
  let failf fmt = Format.kasprintf fail fmt

  (* Query combinatores *)

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

  let atom p = function `A (a, _) -> a | s -> err_exp_atom p s

  let parsed_atom ~kind parse p = function
  | `A (a, _) as s -> (match parse a with Ok v -> v | Error m -> fail m p s)
  | s -> err_exp kind p s

  let enum ~kind ss p = function
  | `A (a, _) when Sset.mem a ss -> a
  | `A (a, l) ->
      let ss = Sset.elements ss in
      let did_you_mean = Tdec.err_did_you_mean ~kind pp_quote in
      let suggestions = match Tdec.err_suggest ss a with
      | [] -> ss | ss -> ss
      in
      errf p l "%a" did_you_mean (a, suggestions)
  | s -> err_exp kind p s

  let enum_map ~kind sm p = function
  | `A (a, l) ->
      begin match Smap.find a sm with
      | v -> v
      | exception Not_found ->
          let ss = Smap.fold (fun k _ acc -> k :: acc) sm [] in
          let did_you_mean = Tdec.err_did_you_mean ~kind pp_quote in
          let suggs = match Tdec.err_suggest ss a with [] -> ss | ss -> ss in
          errf p l "%a" did_you_mean (a, suggs)
      end
  | s -> err_exp kind p s

  let parsed_exn_atom ~kind parse p s =
    let err p l a kind = errf p l "%s: could not parse %s" (esc_atom a) kind in
    match s with
    | `A (a, l) -> (try parse a with Failure _ -> err p l a kind)
    | s -> err_exp kind p s

  let bool p s =
    let tf = "true or false" in
    match s with
    | `A (a, l) ->
        begin match a with
        | "true" -> true
        | "false" -> false
        | a -> err_exp_fnd_raw tf p l (esc_atom a)
        end
    | s -> err_exp tf p s

  let int = parsed_exn_atom ~kind:"integer" int_of_string
  let int32 = parsed_exn_atom ~kind:"int32" Int32.of_string
  let int64 = parsed_exn_atom ~kind:"int64" Int64.of_string
  let float = parsed_exn_atom ~kind:"float" float_of_string

  (* List queries *)

  let is_empty p = function `L (vs, l) -> vs = [] | s -> err_exp_list p s
  let hd q p = function
  | `L ([], l) -> err_empty_list p l
  | `L (v :: _, l) -> q ((`L, l) :: p) v
  | s -> err_exp_list p s

  let tl q p = function
  | `L ([], l) -> err_empty_list p l
  | `L (_ :: [], l) -> q p (`L ([], Tloc.to_end l))
  | `L (_ :: (v :: _ as s), l) ->
      let l = Tloc.with_start (Tloc.to_start (Sexp.loc v)) l in
      q p (`L (s, l))
  | s -> err_exp_list p s

  let nth n q p = function
  | `L (vs, l) ->
      let p = (`L, l) :: p in
      let k, vs = if n < 0 then -n - 1, List.rev vs else n, vs in
      let rec loop k = function
      | v :: vs when k = 0 -> q p v
      | _ :: vs -> loop (k - 1) vs
      | [] -> errf p l "%d: no such index in list" n
      in
      loop k vs
  |  s -> err_exp_list p s

  let fold_list f q acc p = function
  | `L (vs, l) ->
      let p = (`L, l) :: p in
      let add p acc v = f (q p v) acc in
      List.fold_left (add p) acc vs
  | s -> err_exp_list p s

  let list qe = map List.rev (fold_list (fun v acc -> v :: acc) qe [])

  (* Dictionaries *)

  let err_exp_dict = err_exp "dictionary"

  let dict_find bs k =
    let bs = List.rev bs in (* last one takes over. *)
    let rec loop = function
    | `L (`A (a, _) :: vs, l) :: _ when String.equal a k ->
        let vsl = match vs with
        | `L (_, s) :: _ | `A (_, s) :: _ -> Tloc.with_start s l
        | [] -> Tloc.to_end l
        in
        Some (l, `L (vs, vsl))
    | _ :: bs -> loop bs
    | [] -> None
    in
    loop bs

  let dict_dom bs =
    let rec loop dom = function
    | `L (`A (a, _) :: _, _) :: vs -> loop (Sset.add a dom) vs
    | _ :: bs -> loop dom bs
    | [] -> dom
    in
    loop Sset.empty bs

  let key k q p = function
  | `L (bs, dl) ->
      begin match dict_find bs k with
      | Some (kl, v) -> q ((`K k, kl) :: p) v
      | None ->
          let dom = dict_dom bs in
          let keys = Sset.elements dom in
          let pre ppf () = Format.pp_print_string ppf "unbound" in
          let did_you_mean = Tdec.err_did_you_mean ~pre ~kind:"key" pp_key in
          errf p dl "%a" did_you_mean (k, Tdec.err_suggest keys k)
      end
  | s -> err_exp_dict p s

  let opt_key k q ~absent p = function
  | `L (bs, dl) ->
      begin match dict_find bs k with
      | None -> absent
      | Some (kl, v) -> q ((`K k, kl) :: p) v
      end
  | s -> err_exp_dict p s

  let key_dom ~validate p = function
  | `L (bs, _) ->
      let add_key = match validate with
      | None -> fun p loc k acc -> Sset.add k acc
      | Some dom ->
          fun p loc k acc -> match Sset.mem k dom with
          | true -> Sset.add k acc
          | false ->
              let keys = Sset.elements dom in
              let did_you_mean = Tdec.err_did_you_mean ~kind:"key" pp_key in
              errf p loc "%a" did_you_mean (k, Tdec.err_suggest keys k)
      in
      let add_key validate acc = function
      | `L (`A (k, kloc) :: v, _) -> add_key p kloc k acc
      | `L ([], l) -> err_exp_fnd_raw "(atom ...) list" p l "empty list"
      | `L (_, l) -> err_exp_fnd_raw "(atom ...) list" p l "malformed list"
      | s -> err_exp_list p s
      in
      List.fold_left (add_key validate) Sset.empty bs
  | s -> err_exp_dict p s

  let batom q p = function
  | `A (_, _)  as a -> q p a
  | `L ([`A _ as a], _) -> q p a
  | `L ([], l) -> err_exp_fnd_raw "atom" p l "nothing"
  | `L (_, l) -> err_exp_fnd_raw "an atom" p l "list"

  (* OCaml encoding queries *)

  let option q p = function (* TODO improve *)
  | `A ("none", l) -> None
  | `L ((`A ("some", _) :: v), l) ->
      Some (q ((`K "some", l) (* ? *) :: p) (`L (v, l)))
  | `A (a, l) ->
      err_exp_fnd_raw "none or (some ...)" p l (esc_atom a)
  | `L ((`A (a, _) :: v), l) ->
      err_exp_fnd_raw "none or (some ...)" p l ("(" ^ (esc_atom a))
  | `L (_, l) ->
      err_exp_fnd_raw "none or (some ...)" p l "an arbitrary list"
end

module Sexpl = struct
  type index = Key of string | Nth of int
  type path = index list (* reversed *)
  let start = []
  let key k p = Key k :: p
  let nth i p = Nth i :: p
  let query_path p q =
    let rec loop acc = function
    | Key k :: is -> loop (Sexpq.key k acc) is
    | Nth n :: is -> loop (Sexpq.nth n acc) is
    | [] -> acc
    in
    loop q p

  let query_path_opt p q = failwith "TODO"

  type t =
  | Before of path
  | Onto of path
  | After of path

  let path = function Before p | After p | Onto p -> p
  let edit_loc t = failwith "TODO"

  let err i fmt = Format.kasprintf failwith ("%d: " ^^ fmt) i
  let err_unexp_eoi i = err i "unexpected end of input"
  let err_unexp_char i s = err i "unexpected character: %C" s.[i]
  let err_illegal_char i s = err i "illegal character here: %C" s.[i]
  let err_unexp i s =
    err i "unexpected input: %S" (string_with_index_range ~first:i s)

  let parse_eoi s i max = if i > max then () else err_unexp i s
  let parse_index p s i max =
    let first, stop = match s.[i] with '[' -> i + 1, ']' | _ -> i, '.' in
    let last, next =
      let rec loop stop s i max = match i > max with
      | true -> if stop = ']' then err_unexp_eoi i else (i - 1), i
      | false ->
          let illegal = s.[i] = '[' || (s.[i] = ']' && stop = '.') in
          if illegal then err_illegal_char i s else
          if s.[i] <> stop then loop stop s (i + 1) max else
          (i - 1), if stop = ']' then i + 1 else i
      in
      loop stop s first max
    in
    let idx = string_with_index_range ~first ~last s in
    if idx = "" then err first "illegal empty index" else
    match int_of_string idx with
    | exception Failure _ -> next, key idx p
    | idx -> next, nth idx p

  let path_of_string s =
    let rec loop p s i max =
      if i > max then p else
      let next, p = parse_index p s i max in
      if next > max then p else
      if s.[next] <> '.' then err_unexp_char next s else
      if next + 1 <= max then loop p s (next + 1) max else
      err_unexp_eoi next
    in
    try
      if s = "" then err_unexp_eoi 0 else
      let start = if s.[0] = '.' then 1 else 0 in
      Ok (loop [] s start (String.length s - 1))
    with Failure e -> Error e

  let of_string s =
    let rec loop p s i max =
      if i > max then Onto p else
      let next = i + 1 in
      match s.[i] with
      | 'v' when next <= max && s.[next] = '[' ->
          let next, p = parse_index p s next max in
          parse_eoi s next max; Before p
      | c ->
          let next, p = parse_index p s i max in
          if next > max then Onto p else
          if s.[next] = 'v' then (parse_eoi s (next + 1) max; After p) else
          if s.[next] <> '.' then err_unexp_char next s else
          if next + 1 <= max then loop p s (next + 1) max else
          err_unexp_eoi next
    in
    try
      if s = "" then err_unexp_eoi 0 else
      let start = if s.[0] = '.' then 1 else 0 in
      Ok (loop [] s start (String.length s - 1))
    with Failure e -> Error e
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
