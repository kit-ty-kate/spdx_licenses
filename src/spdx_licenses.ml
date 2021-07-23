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

let parse s =
  let lexbuf = Lexing.from_string s in
  try Some (Parser.main Lexer.main lexbuf)
  with Lexer.Error | Parsing.Parse_error -> None
