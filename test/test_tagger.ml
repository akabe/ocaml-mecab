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
open Test_utils

let check_node_list ~ctxt expected nodes =
  let actual =
    List.map (fun v -> v.Mecab.Tagger.surface) nodes
    |> String.concat "" in
  assert_equal ~printer:string ~ctxt expected actual ;
  List.iter
    (fun node ->
       assert_equal ~printer:string ~cmp:(<>) ~ctxt "" node.Mecab.Tagger.feature)
    nodes

let eos_bos = ["", "BOS/EOS,*,*,*,*,*,*,*,*"]

let expected_pairs_1 = [
  "すもも", "名詞,一般,*,*,*,*,すもも,スモモ,スモモ";
  "も", "助詞,係助詞,*,*,*,*,も,モ,モ";
  "もも", "名詞,一般,*,*,*,*,もも,モモ,モモ";
  "も", "助詞,係助詞,*,*,*,*,も,モ,モ";
  "もも", "名詞,一般,*,*,*,*,もも,モモ,モモ";
  "の", "助詞,連体化,*,*,*,*,の,ノ,ノ";
  "うち", "名詞,非自立,副詞可能,*,*,*,うち,ウチ,ウチ";
]
let expected_tostr_1 =
  expected_pairs_1
  |> List.map (fun (surface, feature) -> surface ^ "\t" ^ feature)
  |> String.concat "\n"
  |> sprintf "%s\nEOS\n"
let expected_tonode_1 = List.concat [eos_bos; expected_pairs_1; eos_bos]

let test_version ctxt =
  let cmp re s = Re.execp (Re_perl.compile_pat re) s in
  let expected = "[0-9]+\\.[0-9]+" in
  let actual = Mecab.Tagger.version () in
  assert_equal ~cmp ~printer:string ~ctxt expected actual

let test_create ctxt =
  let mecab = Mecab.Tagger.create [|""|] in
  assert_equal ~cmp:(<>) ~printer:int ~ctxt 0 (Obj.magic mecab)

let test_create2 ctxt =
  let mecab = Mecab.Tagger.create2 " " in
  assert_equal ~cmp:(<>) ~printer:int ~ctxt 0 (Obj.magic mecab)

let test_get_set_partial ctxt =
  let mecab = Mecab.Tagger.create [|""|] in
  Mecab.Tagger.set_partial mecab true ;
  assert_equal ~printer:bool ~ctxt true (Mecab.Tagger.get_partial mecab) ;
  Mecab.Tagger.set_partial mecab false ;
  assert_equal ~printer:bool ~ctxt false (Mecab.Tagger.get_partial mecab)

let test_get_set_theta ctxt =
  let mecab = Mecab.Tagger.create [|""|] in
  Mecab.Tagger.set_theta mecab 9.0 ;
  assert_equal ~printer:float ~ctxt 9.0 (Mecab.Tagger.get_theta mecab) ;
  Mecab.Tagger.set_theta mecab 1.0 ;
  assert_equal ~printer:float ~ctxt 1.0 (Mecab.Tagger.get_theta mecab)

let test_get_set_all_morphs ctxt =
  let mecab = Mecab.Tagger.create [|""|] in
  Mecab.Tagger.set_all_morphs mecab true ;
  assert_equal ~printer:bool ~ctxt true (Mecab.Tagger.get_all_morphs mecab) ;
  Mecab.Tagger.set_all_morphs mecab false ;
  assert_equal ~printer:bool ~ctxt false (Mecab.Tagger.get_all_morphs mecab)

let test_get_set_lattice_level ctxt =
  let mecab = Mecab.Tagger.create2 " -l1" in
  Mecab.Tagger.set_lattice_level mecab Mecab.Tagger.MECAB_NBEST ;
  assert_equal ~ctxt Mecab.Tagger.MECAB_NBEST (Mecab.Tagger.get_lattice_level mecab) ;
  Mecab.Tagger.set_lattice_level mecab Mecab.Tagger.MECAB_PROB ;
  assert_equal ~ctxt Mecab.Tagger.MECAB_PROB (Mecab.Tagger.get_lattice_level mecab)

let test_sparse_tostr ctxt =
  let mecab = Mecab.Tagger.create [|""|] in
  let actual = Mecab.Tagger.sparse_tostr mecab "すもももももももものうち" in
  assert_equal ~printer:string ~ctxt expected_tostr_1 actual ;
  let actual = Mecab.Tagger.sparse_tostr mecab ~pos:6 "。。すもももももももものうち" in
  assert_equal ~printer:string ~ctxt expected_tostr_1 actual ;
  let actual = Mecab.Tagger.sparse_tostr mecab ~len:36 "すもももももももものうち。。" in
  assert_equal ~printer:string ~ctxt expected_tostr_1 actual ;
  let actual = Mecab.Tagger.sparse_tostr mecab ~pos:6 ~len:36 "。。すもももももももものうち。。" in
  assert_equal ~printer:string ~ctxt expected_tostr_1 actual

let test_sparse_tonode ctxt =
  let mecab = Mecab.Tagger.create [|""|] in
  let expected = "すもももももももものうち" in
  let actual = Mecab.Tagger.sparse_tonode mecab "すもももももももものうち" in
  check_node_list ~ctxt expected actual ;
  let actual = Mecab.Tagger.sparse_tonode mecab ~pos:6 "。。すもももももももものうち" in
  check_node_list ~ctxt expected actual ;
  let actual = Mecab.Tagger.sparse_tonode mecab ~len:36 "すもももももももものうち。。" in
  check_node_list ~ctxt expected actual ;
  let actual = Mecab.Tagger.sparse_tonode mecab ~pos:6 ~len:36 "。。すもももももももものうち。。" in
  check_node_list ~ctxt expected actual

let test_nbest_sparse_tostr ctxt =
  let mecab = Mecab.Tagger.create [|""|] in
  let actual = Mecab.Tagger.nbest_sparse_tostr mecab ~n:1 "すもももももももものうち" in
  assert_equal ~printer:string ~ctxt expected_tostr_1 actual ;
  let actual = Mecab.Tagger.nbest_sparse_tostr mecab ~n:1 ~pos:6 "。。すもももももももものうち" in
  assert_equal ~printer:string ~ctxt expected_tostr_1 actual ;
  let actual = Mecab.Tagger.nbest_sparse_tostr mecab ~n:1 ~len:36 "すもももももももものうち。。" in
  assert_equal ~printer:string ~ctxt expected_tostr_1 actual ;
  let actual = Mecab.Tagger.nbest_sparse_tostr mecab ~n:1 ~pos:6 ~len:36 "。。すもももももももものうち。。" in
  assert_equal ~printer:string ~ctxt expected_tostr_1 actual

let test_nbest_next_tostr ctxt =
  let mecab = Mecab.Tagger.create [|""|] in
  Mecab.Tagger.nbest_init mecab "すもももももももものうち" ;
  let actual = Mecab.Tagger.nbest_next_tostr mecab in
  assert_equal ~printer:string ~ctxt expected_tostr_1 actual ;
  Mecab.Tagger.nbest_init mecab ~pos:6 "。。すもももももももものうち" ;
  let actual = Mecab.Tagger.nbest_next_tostr mecab in
  assert_equal ~printer:string ~ctxt expected_tostr_1 actual ;
  Mecab.Tagger.nbest_init mecab ~len:36 "すもももももももものうち。。" ;
  let actual = Mecab.Tagger.nbest_next_tostr mecab in
  assert_equal ~printer:string ~ctxt expected_tostr_1 actual ;
  Mecab.Tagger.nbest_init mecab ~pos:6 ~len:36 "。。すもももももももものうち。。" ;
  let actual = Mecab.Tagger.nbest_next_tostr mecab in
  assert_equal ~printer:string ~ctxt expected_tostr_1 actual

let test_nbest_next_tonode ctxt =
  let mecab = Mecab.Tagger.create [|""|] in
  let expected = "すもももももももものうち" in
  Mecab.Tagger.nbest_init mecab expected ;
  let actual = Mecab.Tagger.nbest_next_tonode mecab in
  check_node_list ~ctxt expected actual ;
  Mecab.Tagger.nbest_init mecab ~pos:6 "。。すもももももももものうち" ;
  let actual = Mecab.Tagger.nbest_next_tonode mecab in
  check_node_list ~ctxt expected actual ;
  Mecab.Tagger.nbest_init mecab ~len:36 "すもももももももものうち。。" ;
  let actual = Mecab.Tagger.nbest_next_tonode mecab in
  check_node_list ~ctxt expected actual ;
  Mecab.Tagger.nbest_init mecab ~pos:6 ~len:36 "。。すもももももももものうち。。" ;
  let actual = Mecab.Tagger.nbest_next_tonode mecab in
  check_node_list ~ctxt expected actual

let suite =
  "Tagger" >::: [
    "version" >:: test_version;
    "create" >:: test_create;
    "create2" >:: test_create2;
    "get_set_partial" >:: test_get_set_partial;
    "get_set_theta" >:: test_get_set_theta;
    "get_set_all_morphs" >:: test_get_set_all_morphs;
    "get_set_lattice_level" >:: test_get_set_lattice_level;
    "sparse_tostr" >:: test_sparse_tostr;
    "sparse_tonode" >:: test_sparse_tonode;
    "nbest_sparse_tostr" >:: test_nbest_sparse_tostr;
    "nbest_next_tostr" >:: test_nbest_next_tostr;
    "nbest_next_tonode" >:: test_nbest_next_tonode;
  ]
