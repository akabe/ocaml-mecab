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

#include "mecab_common.h"

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

value caml_copy_mecab_node(const mecab_node_t *node) {
  CAMLparam0();
  CAMLlocal1(ml_node);

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

  CAMLreturn(ml_node);
}

value caml_copy_mecab_node_list(const mecab_node_t *node) {
  CAMLparam0();
  CAMLlocal2(ml_head, ml_tail);

  if (node == NULL) {
    return Val_emptylist;
  } else {
    while (node->next) node = node->next; // move to the last node.

    for (ml_head = Val_emptylist; node != NULL; node = node->prev) {
      ml_head = caml_alloc(2, 0);
      Store_field(ml_head, 0, caml_copy_mecab_node(node));
      Store_field(ml_head, 1, ml_tail);
	  ml_tail = ml_head;
    }

    CAMLreturn(ml_head);
  }
}
