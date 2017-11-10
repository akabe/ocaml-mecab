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

external create : unit -> t
  = "ml_mecab_lattice_create"

(** Initialize a given lattice. *)
external clear : t -> unit
  = "ml_mecab_lattice_clear"

(** {2 Sentense} *)

(** @return [true] if a given lattice has a sentense. *)
external is_available : t -> bool
  = "ml_mecab_lattice_is_available"

(** @return the length of the current sentense. *)
external get_size : t -> int
  = "ml_mecab_lattice_get_size"

(** @return the current sentense to be parsed. *)
external get_sentense : t -> string option
  = "ml_mecab_lattice_get_sentense"

(** Set a given sentense to be parsed. *)
external set_sentense : t -> ?pos:int -> ?len:int -> string -> unit
  = "ml_mecab_lattice_set_sentense"

(** {2 Model parameters} *)

(** @return normalization factor of CRF. *)
external get_z : t -> float
  = "ml_mecab_lattice_get_z"

(** Set normalization factor of CRF. *)
external set_z : t -> float -> unit
  = "ml_mecab_lattice_set_z"

(** @return temparature parameter theta. *)
external get_theta : t -> float
  = "ml_mecab_lattice_get_theta"

(** Set temparature parameter theta. *)
external set_theta : t -> float -> unit
  = "ml_mecab_lattice_set_theta"

(** {2 Request type} *)

type request_type =
  | MECAB_ONE_BEST (** One best result is obtained (default mode) *)
  | MECAB_NBEST (** Set this flag if you want to obtain N best results. *)
  | MECAB_PARTIAL (** Set this flag if you want to enable a partial parsing mode.
                    When this flag is set, the input [|sentence|] needs to be
                    written in partial parsing format.*)
  | MECAB_MARGINAL_PROB (** Set this flag if you want to obtain marginal
                          probabilities. Marginal probability is set in
                          {!Mecab__Node.prob}. The parsing speed will get 3-5
                          times slower than the default mode. *)
  | MECAB_ALTERNATIVE (** Set this flag if you want to obtain alternative
                          results. Not implemented. *)
  | MECAB_ALL_MORPHS (** When this flag is set, the result {!Mecab.Node} list
                         traverses all nodes in the lattice. *)
  | MECAB_ALLOCATE_SENTENCE (** When this flag is set, tagger internally copies
                                the body of passed sentence into internal
                                buffer. *)
[@@deriving sexp]

(** @return the current request types. *)
external get_request_type : t -> request_type list
  = "ml_mecab_lattice_get_request_type"

(** Set given request types. *)
external set_request_type : t -> request_type list -> unit
  = "ml_mecab_lattice_set_request_type"

(** @return [true] if the object has a specified request type. *)
external has_request_type : t -> request_type -> bool
  = "ml_mecab_lattice_has_request_type"

(** Add request type. *)
external add_request_type : t -> request_type list -> unit
  = "ml_mecab_lattice_add_request_type"

(** Remove request type. *)
external remove_request_type : t -> request_type list -> unit
  = "ml_mecab_lattice_remove_request_type"

(** {2 Boundary constraint} *)

type boundary_constraint =
  | MECAB_ANY_BOUNDARY (** The token boundary is not specified. *)
  | MECAB_TOKEN_BOUNDARY (** The position is a strong token boundary. *)
  | MECAB_INSIDE_TOKEN (** The position is not a token boundary. *)
[@@deriving sexp]

external get_boundary_constraint : t -> int -> boundary_constraint
  = "ml_mecab_lattice_get_boundary_constraint"

external set_boundary_constraint : t -> int -> boundary_constraint -> unit
  = "ml_mecab_lattice_set_boundary_constraint"

(** [get_feature_constraint lattice begin_pos end_pos]
    @return the current feature constraint from [begin_pos] until [end_pos]. *)
external get_feature_constraint : t -> int -> string
  = "ml_mecab_lattice_get_feature_constraint"

external set_feature_constraint : t -> int -> int -> string -> unit
  = "ml_mecab_lattice_set_feature_constraint"

(** @return [true] if a lattice has constraint. *)
external has_constraint : t -> bool
  = "ml_mecab_lattice_has_constraint"

(** {2 Parsing result} *)

(** @return string representation of parsed result. *)
external to_string : t -> string option
  = "ml_mecab_lattice_to_string"

(** @return a list of nodes of parsed result. *)
external to_node : t -> Node.t list
  = "ml_mecab_lattice_to_node"

(** Returns a string of N-best solution if available.

    This is an example code of this function:

{[let mecab = Mecab.Tagger.create [|""|] in
let lattice = Mecab.Lattice.create () in
Mecab.Lattice.(set_request_type lattice [MECAB_NBEST]) ;  (* Enable NBEST mode *)
Mecab.Lattice.set_sentense lattice "すもももももももものうち" ;  (* Set a sentense *)
assert(Mecab.Tagger.parse_lattice mecab lattice) ;  (* Parse the sentense in lattice *)
Mecab.Lattice.nbest_to_string lattice ~n:3  (* Obtain 3-best solution *)
|> print_endline
]} *)
external nbest_to_string : t -> n:int -> string option
  = "ml_mecab_lattice_nbest_to_string"
