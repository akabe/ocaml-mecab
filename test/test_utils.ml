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

open Sexplib
open Sexplib.Std
open OUnit2

(*
let int = string_of_int
let bool = string_of_bool
let float = string_of_float

let string str =
  let b = Buffer.create 16 in
  Buffer.add_char b '\"' ;
  String.iter
    (function
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | '\"' -> Buffer.add_string b "\\\""
      | c -> Buffer.add_char b c)
    str ;
  Buffer.add_char b '\"' ;
  Buffer.contents b

let string_option = function
  | None -> "None"
  | Some s -> "Some " ^ string s

let request_type_list lst =
  [%sexp_of: Mecab.Lattice.request_type list] lst
  |> Sexplib.Sexp.to_string
     *)
let is_alnum s =
  let n = String.length s in
  let rec aux b i =
    if i = n then b else match s.[i] with
      | '0'..'9' | 'a'..'z' | 'A'..'Z' -> b
      | _ -> false
  in
  aux true 0

let rec sexp_to_string = function
  | Sexp.Atom str when is_alnum str -> str
  | Sexp.Atom str ->
    let b = Buffer.create 16 in
    Buffer.add_char b '\"' ;
    String.iter (function
        | '\n' -> Buffer.add_string b "\\n"
        | '\r' -> Buffer.add_string b "\\r"
        | '\t' -> Buffer.add_string b "\\t"
        | '\"' -> Buffer.add_string b "\\\""
        | c -> Buffer.add_char b c)
      str ;
    Buffer.add_char b '\"' ;
    Buffer.contents b
  | Sexp.List sexp_lst ->
    let str = String.concat " " (List.map sexp_to_string sexp_lst) in
    "(" ^ str ^ ")"

let require ?msg ?printer ?sexp ?cmp ?ctxt expected actual =
  let printer = match printer with
    | Some _ as priner -> priner
    | None -> match sexp with
      | Some sexp_of -> Some (fun x -> sexp_to_string (sexp_of x))
      | None -> None
  in
  assert_equal ?msg ?printer ?cmp ?ctxt expected actual

let require_bool = require ~sexp:[%sexp_of: bool]
let require_int = require ~sexp:[%sexp_of: int]
let require_float = require ~sexp:[%sexp_of: float]
let require_str = require ~sexp:[%sexp_of: string]
let require_str_opt = require ~sexp:[%sexp_of: string option]
let require_req_types = require ~sexp:[%sexp_of: Mecab.Lattice.request_type list]
