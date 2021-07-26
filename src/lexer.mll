(* SPDX-License-Identifier: MIT *)

{

exception Error

(* TODO: Remove this when upgrading to OCaml >= 4.08 *)
module Option = struct
  let value ~default = function
    | None -> default
    | Some x -> x
end

}

let idstring = ['a'-'z' 'A'-'Z' '0'-'9' '-' '.']+
let blank = [' ' '\t' '\n']+

rule main = parse
  | blank { main lexbuf }
  | '(' { Parser.LPAREN }
  | ')' { Parser.RPAREN }
  | "WITH" { Parser.WITH }
  | "AND" { Parser.AND }
  | "OR" { Parser.OR }
  | ("DocumentRef-" (idstring as x) ':')? "LicenseRef-" (idstring as y) { Parser.REF (Option.value ~default:"" x, y) }
  | idstring as id { Parser.ID id }
  | (idstring as id) '+' { Parser.IDPLUS id }
  | eof { Parser.EOF }
  | _ { raise Error }
