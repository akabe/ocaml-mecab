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

type boundary_constraint =
  | MECAB_ANY_BOUNDARY (** The token boundary is not specified. *)
  | MECAB_TOKEN_BOUNDARY (** The position is a strong token boundary. *)
  | MECAB_INSIDE_TOKEN (** The position is not a token boundary. *)
[@@deriving sexp]

(** {2 Construction} *)

val create : unit -> t

val create_with :
  ?request_type:request_type list ->
  ?pos:int -> ?len:int -> string -> t

(** {2 Sentense} *)

(** Initialize a given lattice. *)
val clear : t -> unit

(** @return [true] if a given lattice has a sentense. *)
val is_available : t -> bool

(** @return the length of the current sentense. *)
val get_size : t -> int

(** @return the current sentense to be parsed. *)
val get_sentense : t -> string option

(** Set a given sentense to be parsed. *)
val set_sentense : t -> ?pos:int -> ?len:int -> string -> unit

(** {2 Model parameters} *)

(** @return normalization factor of CRF. *)
val get_z : t -> float

(** Set normalization factor of CRF. *)
val set_z : t -> float -> unit

(** @return temparature parameter theta. *)
val get_theta : t -> float

(** Set temparature parameter theta. *)
val set_theta : t -> float -> unit

(** {2 Request type} *)

(** @return the current request types. *)
val get_request_type : t -> request_type list

(** Set given request types. *)
val set_request_type : t -> request_type list -> unit

(** @return [true] if the object has a specified request type. *)
val has_request_type : t -> request_type -> bool

(** Add request type. *)
val add_request_type : t -> request_type list -> unit

(** Remove request type. *)
val remove_request_type : t -> request_type list -> unit

(** {2 Constraints} *)

val get_boundary_constraint : t -> int -> boundary_constraint

val set_boundary_constraint : t -> int -> boundary_constraint -> unit

val get_feature_constraint : t -> int -> string

val set_feature_constraint : t -> int -> int -> string -> unit

(** @return [true] if a lattice has boundary or feature constraint. *)
val has_constraint : t -> bool

(** {2 Parsing result} *)

(** @return string representation of parsed result. *)
val to_string : t -> string option

(** @return a list of nodes of parsed result. *)
val to_node : t -> Node.t list

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
val nbest_to_string : t -> n:int -> string option
