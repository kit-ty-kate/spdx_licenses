(* SPDX-License-Identifier: MIT *)

type simple_license =
  | LicenseID of string
  | LicenseIDPlus of string
  | LicenseRef of (string * string)

type t =
  | Simple of simple_license
  | WITH of simple_license * string
  | AND of t * t
  | OR of t * t

type errors = [
  | `InvalidLicenseID of string
  | `InvalidExceptionID of string
  | `ParseError
]

val parse : string -> (t, [> errors]) Result.t

val to_string : t -> string

val valid_license_ids : string list

val valid_exception_ids : string list
