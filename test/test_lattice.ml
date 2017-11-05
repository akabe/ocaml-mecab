(** MeCab --- A MeCab binding for OCaml

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

open Format
open Sexplib.Std
open OUnit2
open Lattice

let int = string_of_int
let bool = string_of_bool
let float = string_of_float
let string s = s
let string_option = function
  | None -> "None"
  | Some s -> sprintf "(Some %s)" s
let request_type = function
  | MECAB_ONE_BEST -> "MECAB_ONE_BEST"
  | MECAB_NBEST -> "MECAB_NBEST"
  | MECAB_PARTIAL -> "MECAB_PARTIAL"
  | MECAB_MARGINAL_PROB -> "MECAB_MARGINAL_PROB"
  | MECAB_ALTERNATIVE -> "MECAB_ALTERNATIVE"
  | MECAB_ALL_MORPHS -> "MECAB_ALL_MORPHS"
  | MECAB_ALLOCATE_SENTENCE -> "MECAB_ALLOCATE_SENTENCE"
let request_type_list lst =
  List.map request_type lst
  |> String.concat "; "
  |> sprintf "[%s]"

let test_create ctxt =
  let lattice = create () in
  assert_equal ~cmp:(<>) ~ctxt 0 (Obj.magic lattice)

let test_get_set_sentense ctxt =
  let lattice = create () in
  let actual = get_sentense lattice in
  assert_equal ~printer:string_option ~ctxt None actual ;
  set_sentense lattice "こんにちは" ;
  assert_equal ~printer:string_option ~ctxt
    (Some "こんにちは") (get_sentense lattice) ;
  set_sentense lattice ~ofs:3 "こんにちは" ;
  assert_equal ~printer:string_option ~ctxt
    (Some "んにちは") (get_sentense lattice) ;
  set_sentense lattice ~len:9 "こんにちは" ;
  assert_equal ~printer:string_option ~ctxt
    (Some "こんにちは") (get_sentense lattice) ;
  set_sentense lattice ~ofs:3 ~len:9 "こんにちは" ;
  assert_equal ~printer:string_option ~ctxt
    (Some "んにちは") (get_sentense lattice)

let test_get_size ctxt =
  let lattice = create () in
  assert_equal ~printer:int ~ctxt 0 (get_size lattice) ;
  set_sentense lattice "こんにちは" ;
  assert_equal ~printer:int ~ctxt 15 (get_size lattice) ;
  set_sentense lattice ~ofs:3 "こんにちは" ;
  assert_equal ~printer:int ~ctxt 12 (get_size lattice) ;
  set_sentense lattice ~len:9 "こんにちは" ;
  assert_equal ~printer:int ~ctxt 9 (get_size lattice) ;
  set_sentense lattice ~ofs:3 ~len:9 "こんにちは" ;
  assert_equal ~printer:int ~ctxt 9 (get_size lattice)

let test_is_available ctxt =
  let lattice = create () in
  assert_equal ~printer:bool ~ctxt false (is_available lattice) ;
  set_sentense lattice "こんにちは" ;
  assert_equal ~printer:bool ~ctxt true (is_available lattice)

let test_clear ctxt =
  let lattice = create () in
  set_sentense lattice "こんにちは" ;
  clear lattice ;
  assert_equal ~printer:bool ~ctxt false (is_available lattice) ;
  assert_equal ~printer:string_option ~ctxt None (get_sentense lattice)

let test_get_set_z ctxt =
  let lattice = create () in
  set_z lattice 1.0 ;
  assert_equal ~printer:float ~ctxt 1.0 (get_z lattice) ;
  set_z lattice 3.0 ;
  assert_equal ~printer:float ~ctxt 3.0 (get_z lattice)

let test_get_set_theta ctxt =
  let lattice = create () in
  set_theta lattice 1.0 ;
  assert_equal ~printer:float ~ctxt 1.0 (get_theta lattice) ;
  set_theta lattice 3.0 ;
  assert_equal ~printer:float ~ctxt 3.0 (get_theta lattice)

let test_get_set_request_type ctxt =
  let lattice = create () in
  set_request_type lattice [] ;
  assert_equal ~printer:request_type_list ~ctxt
    [] (get_request_type lattice) ;
  set_request_type lattice [MECAB_NBEST] ;
  assert_equal ~printer:request_type_list ~ctxt
    [MECAB_NBEST] (get_request_type lattice) ;
  set_request_type lattice [MECAB_ONE_BEST; MECAB_ALLOCATE_SENTENCE] ;
  assert_equal ~printer:request_type_list ~ctxt
    [MECAB_ONE_BEST; MECAB_ALLOCATE_SENTENCE] (get_request_type lattice)

let test_has_request_type ctxt =
  let lattice = create () in
  set_request_type lattice [MECAB_NBEST] ;
  assert_equal ~printer:bool ~ctxt false (has_request_type lattice MECAB_ONE_BEST) ;
  assert_equal ~printer:bool ~ctxt true (has_request_type lattice MECAB_NBEST)

let test_add_request_type ctxt =
  let lattice = create () in
  set_request_type lattice [] ;
  add_request_type lattice MECAB_NBEST ;
  assert_equal ~printer:bool ~ctxt true (has_request_type lattice MECAB_NBEST)

let test_remove_request_type ctxt =
  let lattice = create () in
  set_request_type lattice [MECAB_NBEST; MECAB_PARTIAL] ;
  remove_request_type lattice MECAB_NBEST ;
  assert_equal ~printer:bool ~ctxt false (has_request_type lattice MECAB_NBEST) ;
  assert_equal ~printer:bool ~ctxt true (has_request_type lattice MECAB_PARTIAL)

let test_tostr ctxt =
  let lattice = create () in
  let mecab = MeCab.create [||] in
  set_sentense lattice "すもももももももものうち" ;
  let b = MeCab.parse_lattice mecab lattice in
  assert_equal ~printer:bool ~ctxt true b
  (* assert_equal ~printer:string ~ctxt "" (tostr lattice) *) (* TODO *)

let suite =
  "Lattice" >::: [
    "create" >:: test_create;
    "get_set_sentense" >:: test_get_set_sentense;
    "get_size" >:: test_get_size;
    "is_available" >:: test_is_available;
    "clear" >:: test_clear;
    "get_set_z" >:: test_get_set_z;
    "get_set_theta" >:: test_get_set_theta;
    "get_set_request_type" >:: test_get_set_request_type;
    "has_request_type" >:: test_has_request_type;
    "add_request_type" >:: test_add_request_type;
    "remove_request_type" >:: test_remove_request_type;
    "tostr" >:: test_tostr;
  ]
