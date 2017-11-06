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

type t

type request_type =
  | MECAB_ONE_BEST (** One best result is obtained (default mode) *)
  | MECAB_NBEST (** Set this flag if you want to obtain N best results. *)
  | MECAB_PARTIAL (** Set this flag if you want to enable a partial parsing mode.
                      When this flag is set, the input [sentence] needs to be
                      written in partial parsing format. *)
  | MECAB_MARGINAL_PROB (** Set this flag if you want to obtain marginal
                            probabilities. Marginal probability is set in
                            [MeCab.Node.prob]. The parsing speed will get 3-5
                            times slower than the default mode. *)
  | MECAB_ALTERNATIVE (** Set this flag if you want to obtain alternative
                          results. Not implemented. *)
  | MECAB_ALL_MORPHS (** When this flag is set, the result linked-list traverses
                         all nodes in the lattice. *)
  | MECAB_ALLOCATE_SENTENCE (** When this flag is set, tagger internally copies
                                the body of passed sentence into internal buffer. *)
[@@deriving sexp]

external create : unit -> t
  = "ml_mecab_lattice_create"

external clear : t -> unit
  = "ml_mecab_lattice_clear"

external is_available : t -> bool
  = "ml_mecab_lattice_is_available"

(** @return [true] if a lattice has the next N-best solution. *)
external next : t -> bool
  = "ml_mecab_lattice_next"

(** @return a list of request types. *)
external get_request_type : t -> request_type list
  = "ml_mecab_lattice_get_request_type"

(** Set a list of request types. *)
external set_request_type : t -> request_type list -> unit
  = "ml_mecab_lattice_set_request_type"

(** @return [true] if the object has a specified request type. *)
external has_request_type : t -> request_type -> bool
  = "ml_mecab_lattice_has_request_type"

(** Add a request type. *)
external add_request_type : t -> request_type -> unit
  = "ml_mecab_lattice_add_request_type"

(** Remove a request type. *)
external remove_request_type : t -> request_type -> unit
  = "ml_mecab_lattice_remove_request_type"
(*
external get_bos_node : t -> Node.t
  = "ocaml_mecab_get_bos_node"

external get_eos_node : t -> Node.t
  = "ocaml_mecab_get_eos_node"

external get_all_begin_nodes : t -> Node.t list
  = "ocaml_mecab_get_all_begin_nodes"

external get_all_end_nodes : t -> Node.t list
  = "ocaml_mecab_get_all_end_nodes"

external get_begin_nodes : t -> Node.t list
  = "ocaml_mecab_get_begin_nodes"

external get_end_nodes : t -> Node.t list
  = "ocaml_mecab_get_end_nodes"
    *)
(** @return the current sentense. *)
external get_sentense : t -> string option
  = "ml_mecab_lattice_get_sentense"

(** Set a (sub-)sentense. *)
external set_sentense : t -> ?ofs:int -> ?len:int -> string -> unit
  = "ml_mecab_lattice_set_sentense"

(** @return the current sentense size. *)
external get_size : t -> int
  = "ml_mecab_lattice_get_size"

(** @return the current normalization factor of CRF. *)
external get_z : t -> float
  = "ml_mecab_lattice_get_z"

(** Set a current normalization factor of CRF. *)
external set_z : t -> float -> unit
  = "ml_mecab_lattice_set_z"

(** @return temparature parameter theta. *)
external get_theta : t -> float
  = "ml_mecab_lattice_get_theta"

(** Set a temparature parameter theta. *)
external set_theta : t -> float -> unit
  = "ml_mecab_lattice_set_theta"

external tostr : t -> string
  = "ml_mecab_lattice_tostr"
