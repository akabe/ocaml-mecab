(* MeCab --- A MeCab binding for OCaml

   Copyright (c) 2017 Akinori ABE

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** MeCab lattice object *)

type t

(** {2 Sentense} *)

external clear : t -> unit
  = "ml_mecab_lattice_clear"

external is_available : t -> bool
  = "ml_mecab_lattice_is_available"

external get_size : t -> int
  = "ml_mecab_lattice_get_size"

external get_sentense : t -> string option
  = "ml_mecab_lattice_get_sentense"

external set_sentense : t -> ?pos:int -> ?len:int -> string -> unit
  = "ml_mecab_lattice_set_sentense"

(** {2 Model parameters} *)

external get_z : t -> float
  = "ml_mecab_lattice_get_z"

external set_z : t -> float -> unit
  = "ml_mecab_lattice_set_z"

external get_theta : t -> float
  = "ml_mecab_lattice_get_theta"

external set_theta : t -> float -> unit
  = "ml_mecab_lattice_set_theta"

(** {2 Request type} *)

type request_type =
  | MECAB_ONE_BEST
  | MECAB_NBEST
  | MECAB_PARTIAL
  | MECAB_MARGINAL_PROB
  | MECAB_ALTERNATIVE
  | MECAB_ALL_MORPHS
  | MECAB_ALLOCATE_SENTENCE
[@@deriving sexp]

external get_request_type : t -> request_type list
  = "ml_mecab_lattice_get_request_type"

external set_request_type : t -> request_type list -> unit
  = "ml_mecab_lattice_set_request_type"

external has_request_type : t -> request_type -> bool
  = "ml_mecab_lattice_has_request_type"

external add_request_type : t -> request_type list -> unit
  = "ml_mecab_lattice_add_request_type"

external remove_request_type : t -> request_type list -> unit
  = "ml_mecab_lattice_remove_request_type"

(** {2 Constraints} *)

type boundary_constraint =
  | MECAB_ANY_BOUNDARY
  | MECAB_TOKEN_BOUNDARY
  | MECAB_INSIDE_TOKEN
[@@deriving sexp]

external get_boundary_constraint : t -> int -> boundary_constraint
  = "ml_mecab_lattice_get_boundary_constraint"

external set_boundary_constraint : t -> int -> boundary_constraint -> unit
  = "ml_mecab_lattice_set_boundary_constraint"

external get_feature_constraint : t -> int -> string
  = "ml_mecab_lattice_get_feature_constraint"

external set_feature_constraint : t -> int -> int -> string -> unit
  = "ml_mecab_lattice_set_feature_constraint"

external has_constraint : t -> bool
  = "ml_mecab_lattice_has_constraint"

(** {2 Construction} *)

external create : unit -> t
  = "ml_mecab_lattice_create"

let create_with ?(request_type = [MECAB_ONE_BEST]) ?pos ?len sentense =
  let lattice = create () in
  set_sentense ?pos ?len lattice sentense ;
  set_request_type lattice request_type ;
  lattice

(** {2 Parsing result} *)

external to_string : t -> string option
  = "ml_mecab_lattice_to_string"

external to_node : t -> Node.t list
  = "ml_mecab_lattice_to_node"

external nbest_to_string : t -> n:int -> string option
  = "ml_mecab_lattice_nbest_to_string"
