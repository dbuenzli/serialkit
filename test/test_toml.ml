(*---------------------------------------------------------------------------
   Copyright (c) 2021 The serialkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let log fmt = Format.kfprintf (Fun.const ()) Format.std_formatter (fmt ^^ "@.")
let log_fail pp ~exp ~fnd =
  log "@[<v3>Expected: @[%a@]@,Found: @[%a@]@]" pp exp pp fnd

let main () =
  log "All tests succeeded."

let () = if !Sys.interactive then () else main ()
