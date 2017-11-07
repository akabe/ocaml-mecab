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

type t

type lattice_level =
  | MECAB_1BEST (** Suggest the single best solution *)
  | MECAB_NBEST (** Suggest N-best solution *)
  | MECAB_PROB  (** Separating words with probabilities *)
[@@deriving sexp]

(** {2 Version} *)

external version : unit -> string
  = "ml_mecab_version"

(** {2 Construction} *)

external create : string array -> t
  = "ml_mecab_create"

external create2 : string -> t
  = "ml_mecab_create2"

(** {2 Properties} *)

external strerror : t -> string
  = "ml_mecab_strerror"

external get_partial : t -> bool
  = "ml_mecab_get_partial"

external set_partial : t -> bool -> unit
  = "ml_mecab_set_partial"

external get_theta : t -> float
  = "ml_mecab_get_theta"

external set_theta : t -> float -> unit
  = "ml_mecab_set_theta"

external get_lattice_level : t -> lattice_level
  = "ml_mecab_get_lattice_level"

external set_lattice_level : t -> lattice_level -> unit
  = "ml_mecab_set_lattice_level"

external get_all_morphs : t -> bool
  = "ml_mecab_get_all_morphs"

external set_all_morphs : t -> bool -> unit
  = "ml_mecab_set_all_morphs"

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

(** {2 Parse sentense} *)

type stat =
  | MECAB_NOR_NODE (** Normal node defined in the dictionary. *)
  | MECAB_UNK_NODE (** Unknown node not defined in the dictionary. *)
  | MECAB_BOS_NODE (** Virtual node representing a beginning of the sentence. *)
  | MECAB_EOS_NODE (** Virtual node representing a end of the sentence. *)
  | MECAB_EON_NODE (** Virtual node representing a end of the N-best enumeration. *)
[@@deriving sexp]

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

external sparse_tostr : t -> ?pos:int -> ?len:int -> string -> string
  = "ml_mecab_sparse_tostr"

external sparse_tonode : t -> ?pos:int -> ?len:int -> string -> node list
  = "ml_mecab_sparse_tonode"

external nbest_sparse_tostr
  : t -> n:int -> ?pos:int -> ?len:int -> string -> string
  = "ml_mecab_nbest_sparse_tostr"

external nbest_init : t -> ?pos:int -> ?len:int -> string -> unit
  = "ml_mecab_nbest_init"

external nbest_next_tostr : t -> string
  = "ml_mecab_nbest_next_tostr"

external nbest_next_tonode : t -> node list
  = "ml_mecab_nbest_next_tonode"
