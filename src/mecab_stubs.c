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

static value
caml_copy_substring(const char *str, int len) {
  CAMLparam0();
  CAMLlocal1(substr);

  substr = caml_alloc_string(len);
  memcpy(String_val(substr), str, len);

  CAMLreturn(substr);
}

enum {
  MECAB_NODE_FIELD_SURFACE = 0,
  MECAB_NODE_FIELD_FEATURE,
  MECAB_NODE_FIELD_ID,
  MECAB_NODE_FIELD_RC_ATTR,
  MECAB_NODE_FIELD_LC_ATTR,
  MECAB_NODE_FIELD_POSID,
  MECAB_NODE_FIELD_CHAR_TYPE,
  MECAB_NODE_FIELD_STAT,
  MECAB_NODE_FIELD_ISBEST,
  MECAB_NODE_FIELD_ALPHA,
  MECAB_NODE_FIELD_BETA,
  MECAB_NODE_FIELD_PROB,
  MECAB_NODE_FIELD_WCOST,
  MECAB_NODE_FIELD_COST,
  MECAB_NODE_N_FIELDS
};

static value
caml_copy_mecab_node(const mecab_node_t *node) {
  CAMLparam0();
  CAMLlocal3(ml_head, ml_tail, ml_node);

  if (node == NULL) {
    return Val_emptylist;
  } else {
    while (node->next) node = node->next; // move to the last node.

    for (ml_head = Val_emptylist; node != NULL; node = node->prev, ml_tail = ml_head) {
      ml_node = caml_alloc(MECAB_NODE_N_FIELDS, 0);
      Store_field(ml_node, MECAB_NODE_FIELD_SURFACE, caml_copy_substring(node->surface, node->length));
      Store_field(ml_node, MECAB_NODE_FIELD_FEATURE, caml_copy_string(node->feature));
      Store_field(ml_node, MECAB_NODE_FIELD_ID, Val_int(node->id));
      Store_field(ml_node, MECAB_NODE_FIELD_RC_ATTR, Val_int(node->rcAttr));
      Store_field(ml_node, MECAB_NODE_FIELD_LC_ATTR, Val_int(node->lcAttr));
      Store_field(ml_node, MECAB_NODE_FIELD_POSID, Val_int(node->posid));
      Store_field(ml_node, MECAB_NODE_FIELD_CHAR_TYPE, Val_int(node->char_type));
      Store_field(ml_node, MECAB_NODE_FIELD_STAT, Val_int(node->stat));
      Store_field(ml_node, MECAB_NODE_FIELD_ISBEST, Val_bool(node->isbest));
      Store_field(ml_node, MECAB_NODE_FIELD_ALPHA, caml_copy_double(node->alpha));
      Store_field(ml_node, MECAB_NODE_FIELD_BETA, caml_copy_double(node->beta));
      Store_field(ml_node, MECAB_NODE_FIELD_PROB, caml_copy_double(node->prob));
      Store_field(ml_node, MECAB_NODE_FIELD_WCOST, Val_int(node->wcost));
      Store_field(ml_node, MECAB_NODE_FIELD_COST, Val_int(node->cost));

      ml_head = caml_alloc(2, 0);
      Store_field(ml_head, 0, ml_node);
      Store_field(ml_head, 1, ml_tail);
    }

    CAMLreturn(ml_head);
  }
}

static void
mecab_finalize(value ml_mecab) {
  mecab_destroy(Mecab_val(ml_mecab));
}

static struct custom_operations mecab_t_ops = {
 identifier:  "mecab_t",
 finalize:    mecab_finalize,
 compare:     custom_compare_default,
 hash:        custom_hash_default,
 serialize:   custom_serialize_default,
 deserialize: custom_deserialize_default
};

static value
mecab_create_or_die(mecab_t * mecab) {
  CAMLparam0();
  CAMLlocal1(ml_mecab);

  if (mecab == NULL) {
    const char *msg = mecab_strerror(mecab);
    if (msg == NULL || strcmp(msg, "") == 0) {
      caml_failwith("Cannot create MeCab object");
	} else {
      caml_failwith(msg);
	}
  }

  ml_mecab = caml_copy_custom(mecab, &mecab_t_ops);
  CAMLreturn(ml_mecab);
}

CAMLprim value
mecab_create_stub(value ml_argv) {
  CAMLparam1(ml_argv);

  int argc = Wosize_val(ml_argv);
  char *argv[argc];
  for (int i = 0; i < argc; ++i) argv[i] = String_val(Field(ml_argv, i));

  CAMLreturn(mecab_create_or_die(mecab_new(argc, argv)));
}

CAMLprim value
mecab_create2_stub(value ml_argv) {
  CAMLparam1(ml_argv);
  CAMLreturn(mecab_create_or_die(mecab_new2(String_val(ml_argv))));
}

CAMLprim value
mecab_strerror_stub(value ml_mecab) {
  CAMLparam1(ml_mecab);
  CAMLreturn(caml_copy_string(mecab_strerror(Mecab_val(ml_mecab))));
}

CAMLprim value
mecab_get_partial_stub(value ml_mecab) {
  CAMLparam1(ml_mecab);
  CAMLreturn(Val_bool(mecab_get_partial(Mecab_val(ml_mecab))));
}

CAMLprim value
mecab_set_partial_stub(value ml_mecab, value ml_partial) {
  CAMLparam2(ml_mecab, ml_partial);
  mecab_set_partial(Mecab_val(ml_mecab), Bool_val(ml_partial));
  CAMLreturn(Val_unit);
}

CAMLprim value
mecab_get_theta_stub(value ml_mecab) {
  CAMLparam1(ml_mecab);
  CAMLreturn(caml_copy_double(mecab_get_theta(Mecab_val(ml_mecab))));
}

CAMLprim value
mecab_set_theta_stub(value ml_mecab, value ml_theta) {
  CAMLparam2(ml_mecab, ml_theta);
  mecab_set_theta(Mecab_val(ml_mecab), Double_val(ml_theta));
  CAMLreturn(Val_unit);
}

CAMLprim value
mecab_get_lattice_level_stub(value ml_mecab) {
  CAMLparam1(ml_mecab);
  CAMLreturn(Val_int(mecab_get_lattice_level(Mecab_val(ml_mecab))));
}

CAMLprim value
mecab_set_lattice_level_stub(value ml_mecab, value ml_lattice_level) {
  CAMLparam2(ml_mecab, ml_lattice_level);
  mecab_set_lattice_level(Mecab_val(ml_mecab), Int_val(ml_lattice_level));
  CAMLreturn(Val_unit);
}

CAMLprim value
mecab_get_all_morphs_stub(value ml_mecab) {
  CAMLparam1(ml_mecab);
  CAMLreturn(Val_bool(mecab_get_all_morphs(Mecab_val(ml_mecab))));
}

CAMLprim value
mecab_set_all_morphs_stub(value ml_mecab, value ml_all_morphs) {
  CAMLparam2(ml_mecab, ml_all_morphs);
  mecab_set_all_morphs(Mecab_val(ml_mecab), Bool_val(ml_all_morphs));
  CAMLreturn(Val_unit);
}

CAMLprim value
mecab_sparse_tostr_stub(value ml_mecab, value ml_str) {
  CAMLparam2(ml_mecab, ml_str);

  mecab_t *mecab = Mecab_val(ml_mecab);

  const char *res = mecab_sparse_tostr(mecab, String_val(ml_str));
  if (res == NULL) caml_failwith(mecab_strerror(mecab));

  CAMLreturn(caml_copy_string(res));
}

CAMLprim value
mecab_sparse_tostr2_stub(value ml_mecab, value ml_str, value ml_len) {
  CAMLparam3(ml_mecab, ml_str, ml_len);

  mecab_t *mecab = Mecab_val(ml_mecab);

  const char *res = mecab_sparse_tostr2(mecab, String_val(ml_str), Int_val(ml_len));
  if (res == NULL) caml_failwith(mecab_strerror(mecab));

  CAMLreturn(caml_copy_string(res));
}

CAMLprim value
mecab_sparse_tonode_stub(value ml_mecab, value ml_str) {
  CAMLparam2(ml_mecab, ml_str);

  mecab_t *mecab = Mecab_val(ml_mecab);
  const mecab_node_t *node = mecab_sparse_tonode(mecab, String_val(ml_str));
  if (node == NULL) caml_failwith(mecab_strerror(mecab));

  CAMLreturn(caml_copy_mecab_node(node));
}

CAMLprim value
mecab_sparse_tonode2_stub(value ml_mecab, value ml_str, value ml_len) {
  CAMLparam3(ml_mecab, ml_str, ml_len);

  mecab_t *mecab = Mecab_val(ml_mecab);
  const mecab_node_t *node = mecab_sparse_tonode2(mecab, String_val(ml_str), Int_val(ml_len));
  if (node == NULL) caml_failwith(mecab_strerror(mecab));

  CAMLreturn(caml_copy_mecab_node(node));
}

CAMLprim value
mecab_nbest_sparse_tostr_stub(value ml_mecab, value ml_n, value ml_str) {
  CAMLparam3(ml_mecab, ml_n, ml_str);

  mecab_t *mecab = Mecab_val(ml_mecab);

  const char *res = mecab_nbest_sparse_tostr(mecab, Int_val(ml_n), String_val(ml_str));
  if (res == NULL) caml_failwith(mecab_strerror(mecab));

  CAMLreturn(caml_copy_string(res));
}

CAMLprim value
mecab_nbest_sparse_tostr2_stub(value ml_mecab, value ml_n, value ml_str, value ml_len) {
  CAMLparam4(ml_mecab, ml_n, ml_str, ml_len);

  mecab_t *mecab = Mecab_val(ml_mecab);

  const char *res = mecab_nbest_sparse_tostr2(mecab, Int_val(ml_n), String_val(ml_str), Int_val(ml_len));
  if (res == NULL) caml_failwith(mecab_strerror(mecab));

  CAMLreturn(caml_copy_string(res));
}

CAMLprim value
mecab_nbest_init_stub(value ml_mecab, value ml_str) {
  CAMLparam2(ml_mecab, ml_str);

  mecab_t *mecab = Mecab_val(ml_mecab);
  int res = mecab_nbest_init(mecab, String_val(ml_str));
  if (!res) caml_failwith(mecab_strerror(mecab));

  CAMLreturn(Val_unit);
}

CAMLprim value
mecab_nbest_init2_stub(value ml_mecab, value ml_str, value ml_len) {
  CAMLparam3(ml_mecab, ml_str, ml_len);

  mecab_t *mecab = Mecab_val(ml_mecab);
  int res = mecab_nbest_init2(mecab, String_val(ml_str), Int_val(ml_len));
  if (!res) caml_failwith(mecab_strerror(mecab));

  CAMLreturn(Val_unit);
}

CAMLprim value
mecab_nbest_next_tostr_stub(value ml_mecab) {
  CAMLparam1(ml_mecab);

  mecab_t *mecab = Mecab_val(ml_mecab);
  const char *res = mecab_nbest_next_tostr(mecab);
  if (res == NULL) caml_failwith(mecab_strerror(mecab));

  CAMLreturn(caml_copy_string(res));
}

CAMLprim value
mecab_nbest_next_tonode_stub(value ml_mecab) {
  CAMLparam1(ml_mecab);

  mecab_t *mecab = Mecab_val(ml_mecab);
  const mecab_node_t *node = mecab_nbest_next_tonode(mecab);
  if (node == NULL) caml_failwith(mecab_strerror(mecab));

  CAMLreturn(caml_copy_mecab_node(node));
}

CAMLprim value
ml_mecab_parse_lattice(value ml_mecab, value ml_lattice) {
  CAMLparam2(ml_mecab, ml_lattice);
  int b = mecab_parse_lattice(Mecab_val(ml_mecab), Mecab_lattice_val(ml_lattice));
  CAMLreturn(Val_bool(b));
}

enum {
  MECAB_DI_FIELD_FILENAME = 0,
  MECAB_DI_FIELD_CHARSET,
  MECAB_DI_FIELD_SIZE,
  MECAB_DI_FIELD_TYPE,
  MECAB_DI_FIELD_LSIZE,
  MECAB_DI_FIELD_RSIZE,
  MECAB_DI_FIELD_VERSION,
  MECAB_DI_N_FIELDS
};

CAMLprim value
mecab_dictionary_info_stub(value ml_mecab) {
  CAMLparam1(ml_mecab);
  CAMLlocal3(ml_head, ml_tail, ml_node);

  mecab_t *mecab = Mecab_val(ml_mecab);
  const mecab_dictionary_info_t *di = mecab_dictionary_info(mecab);

  for (ml_head = Val_emptylist; di != NULL; di = di->next, ml_tail = ml_head) {
    ml_node = caml_alloc(MECAB_DI_N_FIELDS, 0);
    Store_field(ml_node, MECAB_DI_FIELD_FILENAME, caml_copy_string(di->filename));
    Store_field(ml_node, MECAB_DI_FIELD_CHARSET, caml_copy_string(di->charset));
    Store_field(ml_node, MECAB_DI_FIELD_SIZE, Val_int(di->size));
    Store_field(ml_node, MECAB_DI_FIELD_TYPE, Val_int(di->type));
    Store_field(ml_node, MECAB_DI_FIELD_LSIZE, Val_int(di->lsize));
    Store_field(ml_node, MECAB_DI_FIELD_RSIZE, Val_int(di->rsize));
    Store_field(ml_node, MECAB_DI_FIELD_VERSION, Val_int(di->version));

    ml_head = caml_alloc(2, 0);
    Store_field(ml_head, 0, ml_node);
    Store_field(ml_head, 1, ml_tail);
  }

  CAMLreturn(ml_head);
}

CAMLprim value
mecab_version_stub(value ml_unit) {
  CAMLparam1(ml_unit);
  CAMLreturn(caml_copy_string(mecab_version()));
}
