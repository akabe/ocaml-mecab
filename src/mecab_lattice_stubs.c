/* MeCab --- A MeCab binding for OCaml

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
   SOFTWARE. */

#include "common.h"

static void
mecab_lattice_finalize(value ml_lattice) {
  mecab_lattice_destroy(Mecab_lattice_val(ml_lattice));
}

static struct custom_operations mecab_lattice_ops = {
  .identifier =  "mecab_lattice_t",
  .finalize =    mecab_lattice_finalize,
  .compare =     custom_compare_default,
  .hash =        custom_hash_default,
  .serialize =   custom_serialize_default,
  .deserialize = custom_deserialize_default
};

CAMLprim value
ml_mecab_lattice_create(value ml_unit) {
  CAMLparam1(ml_unit);

  mecab_lattice_t *lattice = mecab_lattice_new();
  if (lattice == NULL) caml_failwith("Cannot create mecab_lattice_t instance");

  CAMLreturn(caml_copy_custom(lattice, &mecab_lattice_ops));
}

CAMLprim value
ml_mecab_lattice_get_sentense(value ml_lattice) {
  CAMLparam1(ml_lattice);
  const char *str = mecab_lattice_get_sentence(Mecab_lattice_val(ml_lattice));
  if (str == NULL) {
	CAMLreturn(Val_none);
  } else {
	CAMLreturn(Val_some(caml_copy_string(str)));
  }
}

CAMLprim value
ml_mecab_lattice_set_sentense(value ml_lattice, value ml_ofs, value ml_len, value ml_str) {
  CAMLparam4(ml_lattice, ml_ofs, ml_len, ml_str);

  const char *str = String_val(ml_str);
  if (ml_ofs != Val_none) str += Int_val(Some_val(ml_ofs));

  if (ml_len == Val_none) {
	mecab_lattice_set_sentence(Mecab_lattice_val(ml_lattice), str);
  } else {
	const int len = Int_val(Some_val(ml_len));
	mecab_lattice_set_sentence2(Mecab_lattice_val(ml_lattice), str, len);
  }

  CAMLreturn(Val_unit);
}

CAMLprim value
ml_mecab_lattice_get_size(value ml_lattice) {
  CAMLparam1(ml_lattice);
  CAMLreturn(Val_int(mecab_lattice_get_size(Mecab_lattice_val(ml_lattice))));
}

CAMLprim value
ml_mecab_lattice_clear(value ml_lattice) {
  CAMLparam1(ml_lattice);
  mecab_lattice_clear(Mecab_lattice_val(ml_lattice));
  CAMLreturn(Val_unit);
}

CAMLprim value
ml_mecab_lattice_is_available(value ml_lattice) {
  CAMLparam1(ml_lattice);
  CAMLreturn(Val_bool(mecab_lattice_is_available(Mecab_lattice_val(ml_lattice))));
}

CAMLprim value
ml_mecab_lattice_next(value ml_lattice) {
  CAMLparam1(ml_lattice);
  CAMLreturn(Val_bool(mecab_lattice_next(Mecab_lattice_val(ml_lattice))));
}

CAMLprim value
ml_mecab_lattice_get_z(value ml_lattice) {
  CAMLparam1(ml_lattice);
  CAMLreturn(caml_copy_double(mecab_lattice_get_z(Mecab_lattice_val(ml_lattice))));
}

CAMLprim value
ml_mecab_lattice_set_z(value ml_lattice, value ml_z) {
  CAMLparam2(ml_lattice, ml_z);
  mecab_lattice_set_z(Mecab_lattice_val(ml_lattice), Double_val(ml_z));
  CAMLreturn(Val_unit);
}

CAMLprim value
ml_mecab_lattice_get_theta(value ml_lattice) {
  CAMLparam1(ml_lattice);
  CAMLreturn(caml_copy_double(mecab_lattice_get_theta(Mecab_lattice_val(ml_lattice))));
}

CAMLprim value
ml_mecab_lattice_set_theta(value ml_lattice, value ml_theta) {
  CAMLparam2(ml_lattice, ml_theta);
  mecab_lattice_set_theta(Mecab_lattice_val(ml_lattice), Double_val(ml_theta));
  CAMLreturn(Val_unit);
}

CAMLprim value
ml_mecab_lattice_get_request_type(value ml_lattice) {
  CAMLparam1(ml_lattice);
  CAMLlocal2(ml_head, ml_tail);

  int request_type = mecab_lattice_get_request_type(Mecab_lattice_val(ml_lattice));

  ml_tail = Val_emptylist;

  for (int bit = 6; bit >= 0; --bit) {
	if ((request_type & (1 << bit)) != 0) {
	  ml_head = caml_alloc(2, 0);
	  Store_field(ml_head, 0, Val_int(bit));
	  Store_field(ml_head, 1, ml_tail);
	  ml_tail = ml_head;
	}
  }

  CAMLreturn(ml_tail);
}

CAMLprim value
ml_mecab_lattice_set_request_type(value ml_lattice, value ml_head) {
  CAMLparam2(ml_lattice, ml_head);

  int request_type = 0;
  for (; ml_head != Val_emptylist; ml_head = Field(ml_head, 1)) {
	int bit = Int_val(Field(ml_head, 0));
	request_type |= 1 << bit;
  }

  mecab_lattice_set_request_type(Mecab_lattice_val(ml_lattice), request_type);

  CAMLreturn(Val_unit);
}

CAMLprim value
ml_mecab_lattice_has_request_type(value ml_lattice, value ml_bit) {
  CAMLparam1(ml_lattice);
  int request_type = 1 << Int_val(ml_bit);
  int b = mecab_lattice_has_request_type(Mecab_lattice_val(ml_lattice), request_type);
  CAMLreturn(Val_bool(b));
}

CAMLprim value
ml_mecab_lattice_add_request_type(value ml_lattice, value ml_bit) {
  CAMLparam1(ml_lattice);
  int request_type = 1 << Int_val(ml_bit);
  mecab_lattice_add_request_type(Mecab_lattice_val(ml_lattice), request_type);
  CAMLreturn(Val_unit);
}

CAMLprim value
ml_mecab_lattice_remove_request_type(value ml_lattice, value ml_bit) {
  CAMLparam1(ml_lattice);
  int request_type = 1 << Int_val(ml_bit);
  mecab_lattice_remove_request_type(Mecab_lattice_val(ml_lattice), request_type);
  CAMLreturn(Val_unit);
}

CAMLprim value
ml_mecab_lattice_tostr(value ml_lattice) {
  CAMLparam1(ml_lattice);
  const char *str = mecab_lattice_tostr(Mecab_lattice_val(ml_lattice));
  CAMLreturn(caml_copy_string(str));
}
