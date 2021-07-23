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
