(* SPDX-License-Identifier: MIT *)

(** simple-expression *)
type simple_license =
  | LicenseID of string (** license-id *)
  | LicenseIDPlus of string (** license-id '+' (the '+' isn't contained in the string) *)
  | LicenseRef of (string * string) (** A SPDX user defined license reference.
                                        The first string is the document reference and can be empty.
                                        The second string is the license reference and is NOT empty. *)

(** license-expression *)
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
(** [parse str] parses [str] according to the syntax described in:
    https://spdx.github.io/spdx-spec/appendix-IV-SPDX-license-expressions/ *)

val to_string : t -> string
(** [to_string license] returns a normalized string corresponding to [license]
    in a valid SPDX license expression format. *)

val valid_license_ids : string list
(** [valid_license_ids] gives the list of valid license IDs.
    The list does not contain deprecated licenses.
    See Appendix I.1: https://spdx.github.io/spdx-spec/appendix-I-SPDX-license-list/ *)

val valid_exception_ids : string list
(** [valid_exception_ids] gives the list of valid exception IDs.
    The list does not contain deprecated exceptions.
    See Appendix I.2: https://spdx.github.io/spdx-spec/appendix-I-SPDX-license-list/ *)
