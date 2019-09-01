Layout preserving edits via AST 
-------------------------------

Goal, have a natural, high-level data structure for processing. But
still allow user layout preserving edits.

* In sexpm the idea was to patch/diff sequences of lexemes with a lexeme
  for whitespace. Not sure this is a good approach. It was not fully
  pursued but it seems hard to reconcline in the high-level API.
  
* In serialk for now we keep very precise AST location and we do raw string 
  surgery. Though this can be encapsulated in higher-level abstractions it 
  remains rather brittle and subject to bugs in location tracking. In the
  latter case it makes it easy to generate syntax errors.

* A better approach may be to define a notion of whitespace
  attachement and replace the `Sexp.loc` type in the `Sexp.t` case by
  a `Sexp.src` which has both the location and the attached whitespace
  (and the info whether an atom was quoted or not). Now we can we have
  a `preserve`ing output procedure that spits out the attached
  whitespace when it serializes.
      
  Whitespace attachement likely remains before/after lexeme based. The only 
  problem is how to disambuigate them so that they are uniquely attached: 
  ```
  (a b c ; here's a comment
  )
  ```
  Tentative:
  1. Before/After '(' -> list before/after lsep
  2. Before ')' -> list before rsep
  3. Before atom -> before atom 
  
* Look into `ocamlformat`

* Having syntactically inexisting lists e.g. toplevel sexp list and or
  sexp key bindings as defined as serialk make things more difficult. 
  But they seem more natural to the end user.
