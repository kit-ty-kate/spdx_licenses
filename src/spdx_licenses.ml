(* SPDX-License-Identifier: MIT *)

(* TODO: Remove this when upgrading to OCaml >= 4.08 *)
module Result = struct
  let bind x f = match x with
    | Result.Ok x -> f x
    | Result.Error _ -> x

  let map f = function
    | Result.Ok x -> Result.Ok (f x)
    | Result.Error _ as x -> x
end

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

let uppercased_valid_license_ids =
  List.map String.uppercase_ascii valid_license_ids

let uppercased_valid_exception_ids =
  List.map String.uppercase_ascii valid_exception_ids

let invalid_license_id id =
  let uppercased_id = String.uppercase_ascii id in
  if List.exists (String.equal uppercased_id) uppercased_valid_license_ids
  then Ok ()
  else Error (`InvalidLicenseID id)

let invalid_exception_id id =
  let uppercased_id = String.uppercase_ascii id in
  if List.exists (String.equal uppercased_id) uppercased_valid_exception_ids
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

let simple_to_string = function
  | LicenseID x -> x
  | LicenseIDPlus x -> x^"+"
  | LicenseRef ("", ref) -> "LicenseRef-"^ref
  | LicenseRef (doc, ref) -> "DocumentRef-"^doc^":"^"LicenseRef-"^ref

let to_string =
  let rec aux ~prev = function
    | Simple x -> simple_to_string x
    | WITH (x, exc) -> simple_to_string x^" WITH "^exc
    | AND (x, y) ->
        let s = aux ~prev:`AND x^" AND "^aux ~prev:`AND y in
        begin match prev with
        | (`None | `AND) -> s
        | `OR -> "("^s^")"
        end
    | OR (x, y) ->
        let s = aux ~prev:`OR x^" OR "^aux ~prev:`OR y in
        begin match prev with
        | (`None | `OR) -> s
        | `AND -> "("^s^")"
        end
  in
  aux ~prev:`None
