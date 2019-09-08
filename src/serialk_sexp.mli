(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** S-expression support.

    The module {!Sexp} has general definitions and a codec for working
    with s-expressions. {!Sexpg} generates s-expressions without going
    through a generic representation. {!Sexpq} queries generic
    representations with combinators.

    A short introduction to s-expressions and the syntax parsed by the
    codec is described {{!sexp_syntax}here}.

    Open this module to use it, this only introduces modules in your scope. *)

(** {1:api API} *)

(** S-expression definitions and codec. *)
module Sexp : sig

  (** {1:meta Meta information} *)

  type loc = Serialk_text.Tloc.t
  (** The type for text locations. *)

  type a_meta
  (** The type for meta information about atoms. *)

  type l_meta
  (** The type for meta information about atoms. *)

  val a_meta_nil : a_meta
  (** [a_meta_nil] is parse information for non-parsed atoms. *)

  val l_meta_nil : l_meta
  (** [l_meta_nil] is parse information for non-parsed lists. *)

  (** {1:sexp S-expressions} *)

  type t = [ `A of string * a_meta | `L of t list * l_meta ]
  (** The type for generic s-expression representations. Either an atom or
      a list. *)

  val loc : t -> loc
  (** [loc s] is [s]'s input location. *)

  (** {1:cons Constructors} *)

  val atom : string -> t
  (** [atom a] is [`A (a, not_a_parse)]. *)

  val list : t list -> t
  (** [list l] is [`L (l, not_l_parse)]. *)

  (** {1:access Accessors} *)

  val to_atom : t -> (string, string) result
  (** [to_atom s] extracts an atom from [s].  If [s] is a list an error
      with the location formatted according to {!Tloc.pp} is
      returned. *)

  val to_list : t -> (t list, string) result
  (** [to_list s] extracts a list from [s]. If [s] is an atom an error
      with the location formatted according to {!Tloc.pp} is
      returned. *)

  val get_atom : t -> string
  (** [get_atom s] is like {!to_atom} but raises {!Invalid_argument}
      if [s] is not an atom. *)

  val get_list : t -> t list
  (** [get_atom s] is like {!to_list} but raises {!Invalid_argument}
      if [s] is not an list. *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an s-expression.

      {b Warning.} Assumes all OCaml strings in the formatted value are
      UTF-8 encoded. *)

  val pp_seq : Format.formatter -> t -> unit
  (** [pp_seq] formats an s-expression but if it is a list the
      outer list separators are not formatted in the output.

      {b Warning.} Assumes all OCaml strings in the formatted value are
      UTF-8 encoded. *)

  val pp_dump : Format.formatter -> t -> unit

  (** {1:codec Codec} *)

  val seq_of_string :
    ?file:Serialk_text.Tloc.fpath -> string -> (t, string) result
  (** [seq_of_string ?file s] parses a {e sequence} of s-expressions from
      [s]. [file] is the file for locations, defaults to ["-"]. The
      sequence is returned as a fake s-expression list that spans from
      the start of the first s-expression in [s] to the end of the
      last one; note that this list does not exist syntactically in
      [s].

      If there are no s-expression in [s] the list is empty its
      location has both start and end positions at byte [0] (which may
      not exist).

      {b Note.} All OCaml strings returned by this function are UTF-8
      encoded. *)

  val seq_to_string : t -> string
  (** [seq_to_string s] encodes [s] to a sequence of s-expressions. If [s]
      is an s-expression list this wrapping list is not syntactically
      represented in the output (see also {!of_string}), use
      [to_string (list [l])] if you want to output [l] as a list.

      {b Warning.} Assumes all OCaml strings in [s] are UTF-8 encoded. *)

  (** {1:sexp_path S-expression paths} *)

  type index =
  | Key of string
  | Nth of int (** *)
  (** The type for indexing operations.
      {ul
      {- [Key k], lookup binding [k] in an s-expression
         {{!sexp_dict}dictionary.}}
      {- [Nth n], lookup zero-based element [n] in a list. If [n]
         is negative, counts the number of elements from the end of the
         list, i.e. [-1] is the last list element.}} *)

  type path = index list
  (** The type for paths, a sequence of indexing operations in {b reverse}
      order. *)

  val pave_index : ?stub:t -> index -> t -> t
  (** [pave_index ~stub i s] is either a dictionary binding of [s] to [k]
      if [i] is [Key k] or a list whose [n]th element is [s] if [i] is
      [Nth n] and previous elements in the list set to [stub] (defaults
      to [atom ""]. *)

  val seq_pave_index : ?stub:t -> index -> t -> t
  (** [seq_pave_index ?stub i s] is like {!pave_index} but if
      [s] is a list it splices the sequence at the given index. *)

  val pave_path : ?stub:t -> path -> t -> t
  (** [pave_path ~stub p s] paves the indexes of [p] with {!pave_index} so
      that [s] becomes indexed by [p]. *)

  val seq_pave_path : ?stub:t -> path -> t -> t
  (** [seq_pave_path ~stub p s] is like {!pave_path} except if [s] is
      a list it splices the sequence at the given index. *)

  val path_of_string : string -> (path, string) result
  (** [path_of_string] parses a path from [s] according to the syntax
      {{!sexp_path_caret}here}. *)

  val pp_path : Format.formatter -> path -> unit
  (** [pp_path] is a formatter for paths. *)

  (** {1:caret Caret} *)

  type caret =
  | Before of path (** The void before the s-expression found by the path. *)
  | Over of path  (** The s-expression found by the path. *)
  | After of path (** The void after the s-expression found by the path. *)
  (** The type for carets. *)

  val caret_path : caret -> path
  (** [caret_path c] is the caret's path. *)

  val caret_of_string : string -> (caret, string) result
  (** [caret_of_string s] parses a caret from [s] according to the
      syntax defined {{!sexp_path_caret}here}. *)

  val repath_caret : path -> caret -> caret
  (** [repath_caret p c] replaces [c]'s path with [p]. *)

  val pave_caret : ?stub:t -> caret -> t -> t
  (** [pave_caret ~stub c s] paves the indexes of caret [c] with
      {!pave_index} so that [s] becomes located by the caret. In the
      [Before] and [After] case the last index is ignored. *)

  val seq_pave_caret : ?stub:t -> caret -> t -> t
  (** [seq_pave_caret ~stub p s] is like {!pave_caret} except if [s]
      is a list it splices the sequence at the given index. *)

  val pp_caret : Format.formatter -> caret -> unit
  (** [pp_caret] is a formatter for carets. *)
end

(** S-expression generation. *)
module Sexpg : sig

  (** {1:gen Generation} *)

  type t
  (** The type for generated s-expressions. *)

  val atom : string -> t
  (** [atom s] is [s] as an atom. *)

  type lyst
  (** The type for generated s-expression lists. *)

  val ls : lyst
  (** [ls] starts a list. *)

  val le : lyst -> t
  (** [le l] ends lists [l]. *)

  val el : t -> lyst -> lyst
  (** [el e l] is list [l] with [e] added at the end. *)

  val el_if : bool -> (unit -> t) -> lyst -> lyst
  (** [el cond v l] is [el (v ()) l] if [cond] is [true] and
      [l] otherwise. *)

  (** {1:derived Derived generators} *)

  val atomf : ('a, Format.formatter, unit, t) format4 -> 'a
  (** [atomf fmt ...] is an atom formatted according to [fmt]. *)

  val bool : bool -> t
  (** [bool b] is [atomf "%b" b]. *)

  val int : int -> t
  (** [int i] is [atomf "%d" i]. *)

  val float : float -> t
  (** [float f] is [atomf "%g" f]. *)

  val float_hex : float -> t
  (** [float_hex f] is [atomf "%h" f]. *)

  val string : string -> t
  (** [string s] is {!atom}. *)

  val option : ('a -> t) -> 'a option -> t
  (** [option some o] is [o] as the [none] atom if [o] is
      [none] and a list starting with [some] atom followed by [some v]
      if [o] is [Some v]. *)

  val list : ('a -> t) -> 'a list -> t
  (** [list el l] is [l] as a list whose elements are generated using
      [el]. *)

  val sexp : Sexp.t -> t
  (** [sexp s] is the s-expression [s] as a generated value. *)

  (** {1:output Output} *)

  val buffer_add : Buffer.t -> t -> unit
  (** [buffer_add b g] adds the generated s-expression value [g] to [b]. *)

  val to_string : t -> string
  (** [to_string g] is the generated s-expression value [g] as a string. *)
end

(** S-expression queries. *)
module Sexpq : sig

  (** {1:query paths Result paths} *)

  type path = (Sexp.index * Sexp.loc) list
  (** The type for query result paths. This is a sequence of indexing
      operations tupled with the index location, in {b reverse}
      order. *)

  val pp_path :
    ?pp_key:(Format.formatter -> string -> unit) -> Format.formatter ->
    path -> unit
  (** [pp_path ~pp_key] formats path using [pp_key] to format the keys. *)

  (** {1:queries Queries} *)

  type 'a t
  (** The type for s-expression queries. A query either succeeds
      against an s-expression with a value of type ['a] or it fails. *)

  val query : 'a t -> Sexp.t -> ('a, string) result
  (** [query q s] is [Ok v] if the query [q] succeeds on [s] and a (multiline)
      [Error e] with location information otherwise. *)

  val query' :
    'a t -> Sexp.t -> ('a, path * Sexp.loc * string) result
  (** [query' q s] is like {!query} except in the error case it
      returns [Error (p, l, e)] with [p] the query path that leads to
      the error, [l] the location of the error and [e] the error
      message. *)

  (** {1:outcome Success and failure} *)

  val succeed : 'a -> 'a t
  (** [succeed v] is a query that succeeds with value [v] on any
      s-expression. *)

  val fail : string -> 'a t
  (** [fail msg] is a query that fails on any s-expression with
      message [msg]. Do not include position information in [msg], this
      is automatically handled by the module. *)

  val failf : ('a, Format.formatter, unit, 'b t) format4 -> 'a
  (** [failf fmt ...] is like {!fail} but formats the message
      according to [fmt]. *)

  (** {1:qcomb Query combinators} *)

  val app : ('a -> 'b) t -> 'a t -> 'b t
  (** [app fq q] queries an s-expression first with [fq] and then with [q]
      and applies the result of latter to the former. *)

  val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t
  (** [f $ v] is [app f v]. *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** [bind q f] queries an s-expression with [q], applies the result to
      [f] and re-queries the s-expression with the result. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f q] is [app (succeed f) q]. *)

  val some : 'a t -> 'a option t
  (** [some q] is [map Option.some q]. *)

  (** {1:qsexp S-expression queries} *)

  val fold : atom:'a t -> list:'a t -> 'a t
  (** [fold ~atom ~list] queries atoms with [atom] and lists with [list]. *)

  val sexp : Sexp.t t
  (** [sexp] queries any s-expression and returns its generic
      representation. *)

  val loc : Sexp.loc t
  (** [loc] is [map Sexp.loc sexp]. *)

  val with_loc : 'a t -> ('a * Sexp.loc) t
  (** [with_loc q] queries with [q] an returns the result with the
      location of the s-expression. *)

  (** {1:qatom Atom queries}

      These queries fail on lists. *)

  val atom : string t
  (** [atom] queries an atom as a string. *)

  val parsed_atom : kind:string -> (string -> ('a, string) result) -> 'a t
  (** [parsed_atom ~kind p] queries and atom and parses it with [p]. In
      case of [Error m] fails with message [m]. [kind] is the kind
      of value parsed, used for the error in case a list is found. *)

  val enum : kind:string ->  Set.Make(String).t -> string t
  (** [enum ~kind ss] queries an atom for one of the element of [ss]
      and fails otherwise. [kind] is for the kind of elements in [ss],
      it used for error reporting. *)

  val enum_map : kind:string -> 'a Map.Make(String).t -> 'a t
  (** [enum_map ~kind sm] queries an atom for it's map in [sm] and
      fails if the atom is not bound in [sm]. [kind] is for the kind
      of elements in [sm], it used for error reporting. *)

  val bool : bool t
  (** [bool] queries an atom for one of [true] or [false]. *)

  val int : int t
  (** [int] queries an atom for an integer value parsed with
      {!int_of_string}. *)

  val int32 : int32 t
  (** [int32] queries an atom for an integer value parsed with
      {!Int32.of_string}. *)

  val int64 : int64 t
  (** [int64] queries an atom for an integer value parsed with
      {!Int64.of_string}. *)

  val float : float t
  (** [float] queries an atom for a float value parsed with
      {!float_of_string}. *)

  (** {1:qlist List queries}

      These queries fail on atoms. *)

  val is_empty : bool t
  (** [is_empty] queries a list for emptyness. *)

  val hd : 'a t -> 'a t
  (** [hd q] queries the head of a list with [q]. Fails on empty lists. *)

  val tl : 'a t -> 'a t
  (** [tail q] queries the tail of a list with [q]. Fails on empty lists. *)

  val nth : int -> 'a t -> 'a t
  (** [nth n q] queries the [n]th index of a list with [q]. If [n]
      is negative counts from the end of the list, so [-1] is the last
      list element. The query fails if the index does not exist or on atoms. *)

  val find_nth : int -> 'a t -> absent:'a -> 'a t
  (** [find_nth n q] is like {!nth} but returns [absent] if the index
      does not exist. *)

  val fold_list : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b t
  (** [fold_list f q acc] queries the elements of a list from left to
      right with [q] and folds the result with [f] starting with
      [acc]. *)

  val list : 'a t -> 'a list t
  (** [list q] queries the elements of a list with [q]. *)

  val lone_atom : 'a t -> 'a t
  (** [lone_atom q] queries an atom or the atom of a singleton list
      with [q]. It fails on empty lists or non-singleton lists.

      This is useful for singleton {{!sexp_dict}dictionary}
      bindings. In error reporting treats the list as if it doesn't
      exist syntactically which is the case in dictionary bindings. *)

  (** {1:qdict Dictionary queries}

      Queries for s-expression {{!sexp}dictionaries}. *)

  val key : string -> 'a t -> 'a t
  (** [key k q] queries the value of key [k] of a dictionary with [q].
      The query fails if [k] is not bound or on atoms. *)

  val find_key : string -> 'a t -> absent:'a -> 'a t
  (** [find_key k q ~absent] queries the value of the optional key [k]
      of a dictionary with [q] and uses [absent] if [k] is not bound.
      The query fails on atoms. *)

  val key_dom : validate:Set.Make(String).t option -> Set.Make(String).t t
  (** [key_dom validate] queries the key domain of a list of bindings.
      If [validate] is [Some dom], the query fails if a key is not in
      [dom]. The query also fails if a binding is not well-formed.

      {b XXX} Would be nice to provide support for deprecation. *)

  (** {1:path_caret Path and caret queries} *)

  val index : Sexp.index -> 'a t -> 'a t
  (** [index i q] queries the s-expression index [i] with [q]. This is
      basically {!key} or {!nth} according to [i]. *)

  val find_index : Sexp.index -> 'a t -> absent:'a -> 'a t
  (** [find_index i q] is like {!index} but returns [absent] if the
      index does not exist. This is basically {!find_key} or
      {!find_nth} according to [i]. *)

  val path : Sexp.path -> 'a t -> 'a t
  (** [path p q] queries the s-expression indexed by [p] using
      [q]. The query fails if the path cannot be followed. *)

  val find_path : Sexp.path -> 'a t -> absent:'a -> 'a t
  (** [find_path p q] queries the value indexed by [p] using [q], the
      query succeeds with [absent] if the path doesn't exist. *)

  val probe_path : Sexp.path -> (Sexp.loc * Sexp.path) t
  (** [probe_path p] is a query that probes for [p]'s existence.  It
      succeeds with either the location of the s-expression found by
      [p] and empty path, or the location of the s-expression that
      couldn't be indexed and the remaining path. *)

  (** {1:ocaml OCaml datatype encoding queries} *)

  val option : 'a t -> 'a option t
  (** [option q] queries with [q] the value of an option represented
      according the encoding of {!Sexpg.option}. *)
end

(** {1:sexp_dict Dictionaries}

    An s-expression {e dictionary} is a list of bindings. A {e
    binding} is a list that starts with a {e key} and the remaining
    elements of the list are the binding's {e value}. For example in this
    binding:
{v
(key v0 v1 ...)
v}
    The key is [key] and the value the possibly empty list [v0], [v1],
    ... of s-expressions. The {{!Sexpq.qdict}API} for dictionaries
    represents the value by a fake (doesn't exist syntactically)
    s-expression list whose text location starts at the first element
    of the value.

    {1:sexp_path_caret Path & caret syntax}

    Path and carets provide a way for end users to address
    s-expressions and edit locations.

    A {e path} is a sequence of {{!sexp_dict}key} and list indexing
    operations. Applying the path to an s-expression leads to an
    s-expression or nothing if one of the indices does not exist.

    A {e caret} is a path and a spatial specification for the
    s-expression found by the path. The caret indicates either the
    void before that expression, the expression itself or the void
    after it.

    Here are a few examples of paths and carets, syntactically the
    charater ['v'] is used to denote the caret's insertion point. If there
    is not caret character this is either a path or an {{!Sexp.Over}over} caret.

{v
ocaml.deps        # value of key 'deps' of dictionary 'ocaml'
ocaml.v[deps]     # before the key binding
ocaml.[deps]v     # after the key binding

ocaml.deps.[0]    # first element of key 'deps' of dictionary 'ocaml'
ocaml.deps.v[0]   # before first element (if any)
ocaml.deps.[0]v   # after first element (if any)

ocaml.deps.[-1]   # last element of key 'deps' of dictionary 'ocaml'
ocaml.deps.v[-1]  # before last element (if any)
ocaml.deps.[-1]v  # after last element (if any)
v}

    More formally a {e path} is a [.] seperated list of indices.

    An {e index} is written [[i]] with [i] either a zero-based list
    index (with negative indices counting from the end of the list,
    [-1] is the last element) or a dictionary key [key]; in the latter
    case if there is no ambiguity, the surrounding brackets can be
    dropped.

    A caret is a path whose last index brackets can be prefixed or suffixed
    by the character ['v'] to respectively denote the void before or after
    the s-expression found by the path.

    {b Note.} The syntax has no form of quoting at the moment this
    means key names can't contain, [\[], [\]], or be numbers.

    {1:sexp_syntax S-expression syntax}

    S-expressions are a general way of describing data via atoms
    (sequences of characters) and lists delimited by parentheses.
    Here are a few examples of s-expressions and their syntax:

{v
this-is-an_atom
(this is a list of seven atoms)
(this list contains (a nested) list)

; This is a comment
; Anything that follows a semi-colon is ignored until the next line

(this list ; has three atoms and an embeded ()
 comment)

"this is a quoted atom, it can contain spaces ; and ()"

"quoted atoms can be split ^
 across lines or contain Unicode esc^u{0061}pes"
v}

    We define the syntax of s-expressions over a sequence of
    {{:http://unicode.org/glossary/#unicode_scalar_value}Unicode
    characters} in which all US-ASCII control characters
    (U+0000..U+001F and U+007F) except {{!whitespace}whitespace} are
    forbidden in unescaped form.

    {2:sexp S-expressions}

    An {e s-expression} is either an {{!atoms}{e atom}} or a
    {{!lists}{e list}} of s-expressions interspaced with
    {{!whitespace}{e whitespace}} and {{!comments}{e comments}}. A {e
    sequence of s-expressions} is a succession of s-expressions
    interspaced with whitespace and comments.

    These elements are informally described below and finally made
    precise via an ABNF {{!grammar}grammar}.

    {2:whitespace Whitespace}

    Whitespace is a sequence of whitespace characters, namely, space
    [' '] (U+0020), tab ['\t'] (U+0009), line feed ['\n'] (U+000A),
    vertical tab ['\t'] (U+000B), form feed (U+000C) and carriage return
    ['\r'] (U+000D).

    {2:comments Comments}

    Unless it occurs inside an atom in quoted form (see below)
    anything that follows a semicolon [';'] (U+003B) is ignored until
    the next {e end of line}, that is either a line feed ['\n'] (U+000A), a
    carriage return ['\r']  (U+000D) or a carriage return and a line feed
    ["\r\n"] (<U+000D,U+000A>).

{v
(this is not a comment) ; This is a comment
(this is not a comment)
v}

    {2:atoms Atoms}

    An atom represents ground data as a string of Unicode characters.
    It can, via escapes, represent any sequence of Unicode characters,
    including control characters and U+0000. It cannot represent an
    arbitrary byte sequence except via a client-defined encoding
    convention (e.g. Base64 or hex encoding).

    Atoms can be specified either via an unquoted or a quoted form. In
    unquoted form the atom is written without delimiters. In quoted
    form the atom is delimited by double quote ['"'] (U+0022)
    characters, it is mandatory for atoms that contain
    {{!whitespace}whitespace}, parentheses ['('] [')'], semicolons
    [';'], quotes ['"'], carets ['^'] or characters that need to be
    escaped.

{v
abc        ; a token for the atom "abc"
"abc"      ; a quoted token for the atom "abc"
"abc; (d"  ; a quoted token for the atom "abc; (d"
""         ; the quoted token for the atom ""
v}

    For atoms that do not need to be quoted, both their unquoted and
    quoted form represent the same string; e.g. the string ["true"]
    can be represented both by the atoms {e true} and {e "true"}. The
    empty string can only be represented in quoted form by {e ""}.

    In quoted form escapes are introduced by a caret ['^']. Double
    quotes ['"'] and carets ['^'] must always be escaped.

{v
"^^"             ; atom for ^
"^n"             ; atom for line feed U+000A
"^u{0000}"       ; atom for U+0000
"^"^u{1F42B}^""  ; atom with a quote, U+1F42B and a quote
v}

    The following escape sequences are recognized:
    {ul
    {- ["^ "] (<U+005E,U+0020>) for space [' '] (U+0020)}
    {- ["^\""] (<U+005E,U+0022>) for double quote ['"'] (U+0022)
       {b mandatory}}
    {- ["^^"] (<U+005E,U+005E>) for caret ['^'] (U+005E) {b mandatory}}
    {- ["^n"] (<U+005E,U+006E>) for line feed ['\n'] (U+000A)}
    {- ["^r"] (<U+005E,U+0072>) for carriage return ['\r'] (U+000D)}
    {- ["^u{X}"] with [X] is from 1 to at most 6 upper or lower case
       hexadecimal digits standing for the corresponding
       {{:http://unicode.org/glossary/#unicode_scalar_value}Unicode character}
         U+X.}
    {- Any other character except line feed ['\n'] (U+000A) or
       carriage return ['\r'] (U+000D), following a caret is an
       illegal sequence of characters. In the two former cases the
       atom continues on the next line and white space is ignored.}}

    An atom in quoted form can be split across lines by using a caret
    ['^'] (U+005E) followed by a line feed ['\n'] (U+000A) or a
    carriage return ['\r'] (U+000D); any subsequent
    {{!whitespace}whitespace} is ignored.

{v
"^
  a^
  ^ " ; the atom "a "
v}

    The character ^ (U+005E) is used as an escape character rather
    than the usual \ (U+005C) in order to make quoted Windows®
    file paths decently readable and, not the least, utterly please
    DKM.

    {2:lists Lists}

    Lists are delimited by left ['('] (U+0028) and right [')']
    (U+0029) parentheses. Their elements are s-expressions separated
    by optional {{!whitespace}whitespace} and
    {{!comments}comments}. For example:

{v
(a list (of four) expressions)
(a list(of four)expressions)
("a"list("of"four)expressions)
(a list (of ; This is a comment
four) expressions)
() ; the empty list
v}

    {2:grammar Formal grammar}

    The following {{:https://tools.ietf.org/html/rfc5234}RFC 5234}
    ABNF grammar is defined on a sequence of
    {{:http://unicode.org/glossary/#unicode_scalar_value}Unicode characters}.

{v
 sexp-seq = *(ws / comment / sexp)
     sexp = atom / list
     list = %x0028 sexp-seq %x0029
     atom = token / qtoken
    token = t-char *(t-char)
   qtoken = %x0022 *(q-char / escape / cont) %x0022
   escape = %x005E (%x0020 / %x0022 / %x005E / %x006E / %x0072 /
                    %x0075 %x007B unum %x007D)
     unum = 1*6(HEXDIG)
     cont = %x005E nl ws
       ws = *(ws-char)
  comment = %x003B *(c-char) nl
       nl = %x000A / %x000D / %x000D %x000A
   t-char = %x0021 / %x0023-0027 / %x002A-%x003A / %x003C-%x005D /
            %x005F-%x007E / %x0080-D7FF / %xE000-10FFFF
   q-char = t-char / ws-char / %x0028 / %x0029 / %x003B
  ws-char = %x0020 / %x0009 / %x000A / %x000B / %x000C / %x000D
   c-char = %x0009 / %x000B / %x000C / %x0020-D7FF / %xE000-10FFFF
v}

    A few additional constraints not expressed by the grammar:
    {ul
    {- [unum] once interpreted as an hexadecimal number must be a
       {{:http://unicode.org/glossary/#unicode_scalar_value}Unicode scalar
       value.}}
    {- A comment can be ended by the end of the character sequence rather
       than [nl]. }}
*)
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
