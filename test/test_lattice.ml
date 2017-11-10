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

open Format
open Sexplib.Std
open OUnit2
open Mecab
open Test_utils

let test_create ctxt =
  let lat = Lattice.create () in
  assert_equal ~cmp:(<>) ~ctxt ~msg:"not null" 0 (Obj.magic lat)

let test_get_set_sentense ctxt =
  let lat = Lattice.create () in
  let expected = None in
  assert_equal ~printer:string_option ~ctxt expected (Lattice.get_sentense lat) ;
  Lattice.set_sentense lat "こんにちは" ;
  let expected = Some "こんにちは" in
  assert_equal ~printer:string_option ~ctxt expected (Lattice.get_sentense lat) ;
  Lattice.set_sentense lat ~pos:6 "。。こんにちは" ;
  let expected = Some "こんにちは" in
  assert_equal ~printer:string_option ~ctxt expected (Lattice.get_sentense lat) ;
  Lattice.set_sentense lat ~len:9 "こんにちは" ;
  let expected = Some "こんに" in
  assert_equal ~printer:string_option ~ctxt expected (Lattice.get_sentense lat) ;
  Lattice.set_sentense lat ~pos:6 ~len:9 "。。こんにちは" ;
  let expected = Some "こんに" in
  assert_equal ~printer:string_option ~ctxt expected (Lattice.get_sentense lat)

let test_get_size ctxt =
  let lat = Lattice.create () in
  let expected = 0 in
  assert_equal ~printer:int ~ctxt expected (Lattice.get_size lat) ;
  Lattice.set_sentense lat "こんにちは" ;
  let expected = 15 in
  assert_equal ~printer:int ~ctxt expected (Lattice.get_size lat) ;
  Lattice.set_sentense lat ~pos:6 "。。こんにちは" ;
  let expected = 15 in
  assert_equal ~printer:int ~ctxt expected (Lattice.get_size lat) ;
  Lattice.set_sentense lat ~len:9 "こんにちは" ;
  let expected = 9 in
  assert_equal ~printer:int ~ctxt expected (Lattice.get_size lat) ;
  Lattice.set_sentense lat ~pos:6 ~len:9 "。。こんにちは" ;
  let expected = 9 in
  assert_equal ~printer:int ~ctxt expected (Lattice.get_size lat)

let test_is_available ctxt =
  let lat = Lattice.create () in
  assert_equal ~printer:bool ~ctxt false (Lattice.is_available lat) ;
  Lattice.set_sentense lat "こんにちは" ;
  assert_equal ~printer:bool ~ctxt true (Lattice.is_available lat)

let test_clear ctxt =
  let lat = Lattice.create () in
  Lattice.set_sentense lat "こんにちは" ;
  Lattice.clear lat ;
  assert_equal ~printer:bool ~ctxt false (Lattice.is_available lat)

let test_get_set_z ctxt =
  let lat = Lattice.create () in
  Lattice.set_z lat 1.0 ;
  assert_equal ~printer:float ~ctxt 1.0 (Lattice.get_z lat) ;
  Lattice.set_z lat 2.0 ;
  assert_equal ~printer:float ~ctxt 2.0 (Lattice.get_z lat)

let test_get_set_theta ctxt =
  let lat = Lattice.create () in
  Lattice.set_theta lat 1.0 ;
  assert_equal ~printer:float ~ctxt 1.0 (Lattice.get_theta lat) ;
  Lattice.set_theta lat 2.0 ;
  assert_equal ~printer:float ~ctxt 2.0 (Lattice.get_theta lat)

let test_get_set_request_type ctxt =
  let lat = Lattice.create () in
  let expected = [] in
  Lattice.set_request_type lat expected ;
  assert_equal ~ctxt expected (Lattice.get_request_type lat)
    ~printer:request_type_list ;
  let expected = Lattice.([MECAB_NBEST; MECAB_PARTIAL]) in
  Lattice.set_request_type lat expected ;
  assert_equal ~ctxt expected (Lattice.get_request_type lat)
    ~printer:request_type_list ;
  let expected = Lattice.([MECAB_ONE_BEST; MECAB_PARTIAL; MECAB_ALL_MORPHS]) in
  Lattice.set_request_type lat expected ;
  assert_equal ~ctxt expected (Lattice.get_request_type lat)
    ~printer:request_type_list

let test_has_request_type ctxt =
  let lat = Lattice.create () in
  Lattice.set_request_type lat Lattice.([MECAB_NBEST; MECAB_PARTIAL]) ;
  let expected = true in
  let actual = Lattice.has_request_type lat Lattice.MECAB_NBEST in
  assert_equal ~printer:bool ~ctxt expected actual ;
  let expected = true in
  let actual = Lattice.has_request_type lat Lattice.MECAB_PARTIAL in
  assert_equal ~printer:bool ~ctxt expected actual ;
  let expected = false in
  let actual = Lattice.has_request_type lat Lattice.MECAB_ONE_BEST in
  assert_equal ~printer:bool ~ctxt expected actual

let test_add_request_type ctxt =
  let lat = Lattice.create () in
  Lattice.set_request_type lat Lattice.([MECAB_NBEST]) ;
  Lattice.add_request_type lat [] ;
  let expected = Lattice.([MECAB_NBEST]) in
  let actual = Lattice.get_request_type lat in
  assert_equal ~printer:request_type_list ~ctxt expected actual ;
  Lattice.add_request_type lat Lattice.([MECAB_PARTIAL]) ;
  let expected = Lattice.([MECAB_NBEST; MECAB_PARTIAL]) in
  let actual = Lattice.get_request_type lat in
  assert_equal ~printer:request_type_list ~ctxt expected actual ;
  Lattice.add_request_type lat Lattice.([MECAB_ALTERNATIVE; MECAB_ALL_MORPHS]) ;
  let expected = Lattice.([MECAB_NBEST; MECAB_PARTIAL; MECAB_ALTERNATIVE; MECAB_ALL_MORPHS]) in
  let actual = Lattice.get_request_type lat in
  assert_equal ~printer:request_type_list ~ctxt expected actual

let test_remove_request_type ctxt =
  let lat = Lattice.create () in
  Lattice.set_request_type lat
    Lattice.([MECAB_NBEST; MECAB_PARTIAL; MECAB_ALTERNATIVE; MECAB_ALL_MORPHS]) ;
  Lattice.remove_request_type lat [] ;
  let expected = Lattice.([MECAB_NBEST; MECAB_PARTIAL; MECAB_ALTERNATIVE; MECAB_ALL_MORPHS]) in
  let actual = Lattice.get_request_type lat in
  assert_equal ~printer:request_type_list ~ctxt expected actual ;
  Lattice.remove_request_type lat Lattice.([MECAB_PARTIAL]) ;
  let expected = Lattice.([MECAB_NBEST; MECAB_ALTERNATIVE; MECAB_ALL_MORPHS]) in
  let actual = Lattice.get_request_type lat in
  assert_equal ~printer:request_type_list ~ctxt expected actual ;
  Lattice.remove_request_type lat Lattice.([MECAB_ALTERNATIVE; MECAB_ALL_MORPHS]) ;
  let expected = Lattice.([MECAB_NBEST]) in
  let actual = Lattice.get_request_type lat in
  assert_equal ~printer:request_type_list ~ctxt expected actual

let expected_tostr_1 = Some "私\t名詞,代名詞,一般,*,*,*,私,ワタシ,ワタシ\nEOS\n"

let test_to_string ctxt =
  let mecab = Tagger.create [|""|] in
  let lat = Lattice.create () in
  assert_equal ~ctxt ~msg:"before set_sentense" None (Lattice.to_string lat) ;
  Lattice.set_sentense lat "私" ;
  assert_equal ~ctxt ~msg:"before parse_lattice" None (Lattice.to_string lat) ;
  assert_equal ~printer:bool ~ctxt true (Tagger.parse_lattice mecab lat) ;
  let actual = Lattice.to_string lat in
  assert_equal ~printer:string_option ~ctxt expected_tostr_1 actual

let test_to_node ctxt =
  let mecab = Tagger.create [|""|] in
  let lat = Lattice.create () in
  assert_equal ~ctxt ~msg:"before set_sentense" [] (Lattice.to_node lat) ;
  Lattice.set_sentense lat "すもももももももものうち" ;
  assert_equal ~ctxt ~msg:"before parse_lattice" [] (Lattice.to_node lat) ;
  assert_equal ~printer:bool ~ctxt true (Tagger.parse_lattice mecab lat) ;
  let actual = Lattice.to_node lat
               |> List.map (fun v -> v.Node.surface)
               |> String.concat " " in
  assert_equal ~printer:string ~ctxt " すもも も もも も もも の うち " actual

let test_nbest_to_string ctxt =
  let mecab = Tagger.create [|""|] in
  let lat = Lattice.create () in
  let actual = Lattice.nbest_to_string lat ~n:1 in
  assert_equal ~printer:string_option ~ctxt ~msg:"before set_sentense" None actual ;
  Lattice.(set_request_type lat [MECAB_NBEST]) ;
  Lattice.set_sentense lat "私" ;
  let actual = Lattice.nbest_to_string lat ~n:1 in
  assert_equal ~printer:string_option ~ctxt ~msg:"before parse_lattice" None actual ;
  assert_equal ~printer:bool ~ctxt true (Tagger.parse_lattice mecab lat) ;
  let actual = Lattice.nbest_to_string lat ~n:1 in
  assert_equal ~printer:string_option ~ctxt expected_tostr_1 actual

let test_set_boundary_constraint ctxt =
  let mecab = Tagger.create [|""|] in
  let lat = Lattice.create () in
  Lattice.set_sentense lat "こんにちは" ;
  Lattice.(set_boundary_constraint lat 6 MECAB_TOKEN_BOUNDARY) ;
  assert_equal ~printer:bool ~ctxt true (Tagger.parse_lattice mecab lat) ;
  let fst_node = List.nth (Lattice.to_node lat) 1 in
  let actual = fst_node.Node.surface in
  assert_equal ~printer:string ~ctxt "こん" actual

let test_get_boundary_constraint ctxt =
  let lat = Lattice.create () in
  Lattice.set_sentense lat "こんにちは" ;
  Lattice.(set_boundary_constraint lat 6 MECAB_TOKEN_BOUNDARY) ;
  let actual = Lattice.get_boundary_constraint lat 6 in
  assert_equal ~ctxt Lattice.MECAB_TOKEN_BOUNDARY actual

let test_set_feature_constraint ctxt =
  let mecab = Tagger.create [|""|] in
  let lat = Lattice.create () in
  Lattice.set_sentense lat "こんにちは" ;
  Lattice.set_feature_constraint lat 6 9 "DUMMY" ;
  assert_equal ~printer:bool ~ctxt true (Tagger.parse_lattice mecab lat) ;
  let actual = Lattice.to_node lat
               |> List.map (fun v -> v.Node.surface)
               |> String.concat " " in
  assert_equal ~printer:string ~ctxt " こん に ち は " actual

let test_get_feature_constraint ctxt =
  let lat = Lattice.create () in
  Lattice.set_sentense lat "こんにちは" ;
  Lattice.set_feature_constraint lat 6 9 "DUMMY" ;
  let actual = Lattice.get_feature_constraint lat 6 in
  assert_equal ~printer:string ~ctxt "DUMMY" actual

let test_has_constraint ctxt =
  let lat = Lattice.create () in
  Lattice.set_sentense lat "こんにちは" ;
  assert_equal ~printer:bool ~ctxt false (Lattice.has_constraint lat) ;
  Lattice.(set_boundary_constraint lat 6 MECAB_TOKEN_BOUNDARY) ;
  assert_equal ~printer:bool ~ctxt true (Lattice.has_constraint lat)

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
    "to_string" >:: test_to_string;
    "to_node" >:: test_to_node;
    "nbest_to_string" >:: test_nbest_to_string;
    "set_boundary_constraint" >:: test_set_boundary_constraint;
    "get_boundary_constraint" >:: test_get_boundary_constraint;
    "set_feature_constraint" >:: test_set_feature_constraint;
    "get_feature_constraint" >:: test_get_feature_constraint;
    "has_constraint" >:: test_has_constraint;
  ]
