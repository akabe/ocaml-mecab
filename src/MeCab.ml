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

(** MeCab is a part-of-speech and morphological analyzer. *)

open Sexplib.Std

type lattice_level =
  | MECAB_1BEST (** Suggest the single best solution *)
  | MECAB_NBEST (** Suggest N-best solution *)
  | MECAB_PROB  (** Separating words with probabilities *)
[@@deriving sexp]

type stat =
  | MECAB_NOR_NODE (** Normal node defined in the dictionary. *)
  | MECAB_UNK_NODE (** Unknown node not defined in the dictionary. *)
  | MECAB_BOS_NODE (** Virtual node representing a beginning of the sentence. *)
  | MECAB_EOS_NODE (** Virtual node representing a end of the sentence. *)
  | MECAB_EON_NODE (** Virtual node representing a end of the N-best enumeration. *)
[@@deriving sexp]

type t

type node =
  {
    surface : string; (** surface string. *)
    feature : string; (** feature string. *)
    id : int; (** unique node id *)
    rc_attr : int; (** right attribute id *)
    lc_attr : int; (** left attribute id *)
    posid : int; (** unique part of speech id. This value is defined in [pos.def] file. *)
    char_type : int; (** character type *)
    stat : stat; (** status of this model. *)
    isbest : bool; (** [true] if this node is best node. *)
    alpha : float; (** forward accumulative log summation.
                    * This value is only available when [MECAB_MARGINAL_PROB] is passed. *)
    beta : float; (** backward accumulative log summation.
                   * This value is only available when [MECAB_MARGINAL_PROB] is passed. *)
    prob : float; (** marginal probability.
                   * This value is only available when [MECAB_MARGINAL_PROB] is passed. *)
    wcost : int; (** word cost. *)
    cost : int; (** best accumulative cost from bos node to this node. *)
  }
[@@deriving sexp]

external create : string array -> t
  = "mecab_create_stub"

external create2 : string -> t
  = "mecab_create2_stub"

external strerror : t -> string
  = "mecab_strerror_stub"

external get_partial : t -> bool
  = "mecab_get_partial_stub"

external set_partial : t -> bool -> unit
  = "mecab_set_partial_stub"

external get_theta : t -> float
  = "mecab_get_theta_stub"

external set_theta : t -> float -> unit
  = "mecab_set_theta_stub"

external get_lattice_level : t -> lattice_level
  = "mecab_get_lattice_level_stub"

external set_lattice_level : t -> lattice_level -> unit
  = "mecab_set_lattice_level_stub"

external get_all_morphs : t -> bool
  = "mecab_get_all_morphs_stub"

external set_all_morphs : t -> bool -> unit
  = "mecab_set_all_morphs_stub"

external sparse_tostr : t -> string -> string
  = "mecab_sparse_tostr_stub"

external sparse_tostr2 : t -> string -> int -> string
  = "mecab_sparse_tostr2_stub"

external sparse_tonode : t -> string -> node list
  = "mecab_sparse_tonode_stub"

external sparse_tonode2 : t -> string -> int -> node list
  = "mecab_sparse_tonode2_stub"

external nbest_sparse_tostr : t -> n:int -> string -> string
  = "mecab_nbest_sparse_tostr_stub"

external nbest_sparse_tostr2 : t -> n:int -> string -> int -> string
  = "mecab_nbest_sparse_tostr2_stub"

external nbest_init : t -> string -> unit
  = "mecab_nbest_init_stub"

external nbest_init2 : t -> string -> int -> unit
  = "mecab_nbest_init2_stub"

external nbest_next_tostr : t -> string
  = "mecab_nbest_next_tostr_stub"

external nbest_next_tonode : t -> node list
  = "mecab_nbest_next_tonode_stub"

external parse_lattice : t -> Lattice.t -> bool
  = "ml_mecab_parse_lattice"

(** {2 Dictionary information} *)

type dictionary_type =
  | MECAB_SYS_DIC (** This is a system dictionary. *)
  | MECAB_USR_DIC (** This is a user dictionary. *)
  | MECAB_UNK_DIC (** This is a unknown word dictionary. *)
[@@deriving sexp]

type dictionary_info =
  {
    filename : string; (** filename of dictionary.
                           On Windows, filename is stored in UTF-8 encoding. *)
    charset : string; (** character set of the dictionary.
                          e.g., ["SHIFT-JIS"], ["UTF-8"]. *)
    size : int; (** How many words are registered in this dictionary. *)
    dic_type : dictionary_type; (** dictionary type *)
    lsize : int; (** left attributes size *)
    rsize : int; (** right attributes size *)
    version : int; (** version of this dictionary *)
  }
[@@deriving sexp]

external dictionary_info : t -> dictionary_info list
  = "mecab_dictionary_info_stub"

(** {2 Version} *)

external version : unit -> string
  = "mecab_version_stub"
