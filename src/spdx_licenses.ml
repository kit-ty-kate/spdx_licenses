(* SPDX-License-Identifier: MIT *)

type simple_license = Types.simple_license =
  | LicenseID of string
  | LicenseIDPlus of string
  | LicenseRef of (string * string)

type t = Types.t =
  | Simple of simple_license
  | WITH of simple_license * string
  | AND of t * t
  | OR of t * t

type errors = [
  | `InvalidLicenseID of string
  | `InvalidExceptionID of string
  | `ParseError
]

let ( >>= ) = Result.bind

let valid_license_ids = LicenseIDs.list
let valid_exception_ids = ExceptionIDs.list

let invalid_license_id id =
  if List.exists (String.equal id) valid_license_ids
  then Ok ()
  else Error (`InvalidLicenseID id)

let invalid_exception_id id =
  if List.exists (String.equal id) valid_exception_ids
  then Ok ()
  else Error (`InvalidExceptionID id)

let find_invalid_id = function
  | LicenseID id -> invalid_license_id id
  | LicenseIDPlus id -> invalid_license_id id
  | LicenseRef _ -> Ok ()

let rec find_invalid = function
  | Simple license -> find_invalid_id license
  | WITH (simple, exc) ->
      find_invalid_id simple >>= fun () -> invalid_exception_id exc
  | AND (x, y) -> find_invalid x >>= fun () -> find_invalid y
  | OR (x, y) -> find_invalid x >>= fun () -> find_invalid y

let parse s =
  let lexbuf = Lexing.from_string s in
  match Parser.main Lexer.main lexbuf with
  | license -> Result.map (fun () -> license) (find_invalid license)
  | exception (Lexer.Error | Parsing.Parse_error) -> Error `ParseError
