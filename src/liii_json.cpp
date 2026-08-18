//
// Copyright (C) 2026 The Goldfish Scheme Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations
// under the License.
//

#include "s7.h"
#include <cstdlib>
#include <cstring>
#include <cerrno>
#include <vector>
#include <string>

namespace goldfish {

// json->string 的 C++ 实现，语义与历史上 (guenchi json) 中的 Scheme 实现完全一致：
//   - vector   => JSON 数组
//   - 序对列表  => JSON 对象（键为符号时输出宽松格式，即不带引号）
//   - '()      => {}
//   - 字符串    => 转义后带引号输出（\" \\ \/ \b \f \n \r \t，多字节 UTF-8 原样输出）
//   - symbol   => 原样输出（如 true false null）
//   - number   => number->string
//   - boolean  => true / false

static s7_pointer
json_type_error (s7_scheme* sc, const char* msg, s7_pointer arg) {
  return s7_error (sc, s7_make_symbol (sc, "type-error"), s7_list (sc, 2, s7_make_string (sc, msg), arg));
}

static void
json_write_escaped_string (std::string& out, const char* s, s7_int len) {
  out.push_back ('"');
  for (s7_int i= 0; i < len; i++) {
    unsigned char c= (unsigned char) s[i];
    if (c < 0x80) {
      switch (c) {
      case '"':
        out+= "\\\"";
        break;
      case '\\':
        out+= "\\\\";
        break;
      case '/':
        out+= "\\/";
        break;
      case '\b':
        out+= "\\b";
        break;
      case '\f':
        out+= "\\f";
        break;
      case '\n':
        out+= "\\n";
        break;
      case '\r':
        out+= "\\r";
        break;
      case '\t':
        out+= "\\t";
        break;
      default:
        if (c < 0x20) {
          // [0125] 控制字符（含 NUL）转义为 \uXXXX，保证输出可再解析
          char buf[8];
          snprintf (buf, sizeof (buf), "\\u%04x", (unsigned) c);
          out+= buf;
        }
        else {
          out.push_back ((char) c);
        }
        break;
      }
    }
    else {
      // 多字节 UTF-8 字符，直接输出原始字节
      out.push_back ((char) c);
    }
  }
  out.push_back ('"');
}

static void json_write_value (s7_scheme* sc, s7_pointer x, std::string& out);

// [0125] 空对象 '(()) 在顶层与嵌套位置都应输出 {}（此前顶层输出 {{}}）
static bool json_is_null_object (s7_scheme* sc, s7_pointer x);

static void
json_write_scalar (s7_scheme* sc, s7_pointer x, std::string& out) {
  if (s7_is_string (x)) {
    json_write_escaped_string (out, s7_string (x), s7_string_length (x));
  }
  else if (s7_is_number (x)) {
    char* s= s7_number_to_string (sc, x, 10);
    out+= s;
    free (s);
  }
  else if (s7_is_boolean (x)) {
    out+= (s7_boolean (sc, x) ? "true" : "false");
  }
  else if (s7_is_symbol (x)) {
    out+= s7_symbol_name (x);
  }
  else if (s7_is_null (sc, x)) {
    out+= "{}";
  }
  else {
    json_type_error (sc, "Unexpected x: ", x);
  }
}

// 统计序对链的长度；链以非空原子结尾（非真列表）时返回 -1
static s7_int
json_pair_chain_length (s7_scheme* sc, s7_pointer x) {
  s7_int     len= 0;
  s7_pointer p  = x;
  while (s7_is_pair (p)) {
    len++;
    p= s7_cdr (p);
  }
  if (!s7_is_null (sc, p)) return -1;
  return len;
}

static void
json_write_object_entry (s7_scheme* sc, s7_pointer d, std::string& out) {
  if (s7_is_null (sc, d)) {
    out+= "{}";
    return;
  }
  if (!s7_is_pair (d)) {
    s7_error (sc, s7_make_symbol (sc, "value-error"),
              s7_list (sc, 2, d, s7_make_string (sc, " must be null, pair, or list with at least 2 elements")));
    return;
  }
  s7_int len= json_pair_chain_length (sc, d);
  if (!(len == -1 || len >= 2)) {
    s7_error (sc, s7_make_symbol (sc, "value-error"),
              s7_list (sc, 2, d, s7_make_string (sc, " must be null, pair, or list with at least 2 elements")));
    return;
  }
  s7_pointer k= s7_car (d);
  s7_pointer v= s7_cdr (d);
  json_write_scalar (sc, k, out);
  out.push_back (':');
  if (s7_is_null (sc, v)) {
    out+= "{}";
  }
  else if ((s7_is_pair (v) && json_pair_chain_length (sc, v) != -1) || s7_is_vector (v)) {
    json_write_value (sc, v, out);
  }
  else {
    json_write_scalar (sc, v, out);
  }
}

static void
json_write_value (s7_scheme* sc, s7_pointer x, std::string& out) {
  if (json_is_null_object (sc, x)) {
    out+= "{}";
    return;
  }
  if (s7_is_vector (x)) {
    out.push_back ('[');
    s7_int      len  = s7_vector_length (x);
    s7_pointer* elems= s7_vector_elements (x);
    for (s7_int i= 0; i < len; i++) {
      if (i > 0) out.push_back (',');
      s7_pointer k= elems[i];
      if (s7_is_vector (k) || s7_is_pair (k)) {
        json_write_value (sc, k, out);
      }
      else {
        json_write_scalar (sc, k, out);
      }
    }
    out.push_back (']');
  }
  else if (s7_is_pair (x)) {
    out.push_back ('{');
    s7_pointer lst= x;
    s7_int     i  = 0;
    while (s7_is_pair (lst)) {
      if (i > 0) out.push_back (',');
      json_write_object_entry (sc, s7_car (lst), out);
      lst= s7_cdr (lst);
      i++;
    }
    if (!s7_is_null (sc, lst)) {
      s7_error (sc, s7_make_symbol (sc, "value-error"),
                s7_list (sc, 2, lst, s7_make_string (sc, " must be null, pair, or list with at least 2 elements")));
      return;
    }
    out.push_back ('}');
  }
  else {
    json_write_scalar (sc, x, out);
  }
}

static s7_pointer
f_json_to_string (s7_scheme* sc, s7_pointer args) {
  s7_pointer x= s7_car (args);
  if (s7_is_procedure (x)) {
    return s7_error (sc, s7_make_symbol (sc, "type-error"),
                     s7_list (sc, 1, s7_make_string (sc, "json->string: input must not be a procedure")));
  }
  std::string out;
  json_write_value (sc, x, out);
  return s7_make_string_with_length (sc, out.data (), (s7_int) out.size ());
}

static void
glue_json_to_string (s7_scheme* sc) {
  const char* name= "g_json->string";
  const char* desc= "(g_json->string data) => string, encode Scheme-form JSON data to a JSON string";
  s7_define_function (sc, name, f_json_to_string, 1, 0, false, desc);
}

// string->json 的 C++ 实现，语义与历史上 (guenchi json) 中的 Scheme 实现完全一致。
// Scheme 实现的做法是：逐字符扫描 JSON 文本，把 { } [ ] : , 改写为 Scheme 可读形式
// （{ -> "((", } -> "))", [ -> "#(", ] -> ")", : -> " . ", 对象内 , -> ")(", 数组内 , -> " "），
// 字符串内的转义做部分处理（\/ 变为 /，\uXXXX 直接展开为 UTF-8 字节，其余转义原样保留
// 交由 reader 处理），然后对改写后的字符串调用 read。
// 关键行为（必须与 Scheme 版逐字节一致）：
//   - 只在分隔符处落盘：最后一个分隔符之后的内容被丢弃（故顶层标量解析为 eof-object）
//   - 上下文栈初始为 (#t)，loose-cdr 到空后保持为空，空栈顶按真值处理（即对象上下文）
//   - \uXXXX 需要满足 end+6 < len，否则 parse-error "HEX sequence too short ..."
//   - 代理对合并要求 end+12 < len；非法 hex 抛 parse-error "Invalid HEX sequence ..."
//   - 非法转义字符抛 parse-error "Invalid escape char: X"
//   - 码点超出 [0, 1114111] 抛 value-error（与 (liii unicode) codepoint->utf8 一致）

static s7_pointer
json_parse_error (s7_scheme* sc, const std::string& msg) {
  return s7_error (sc, s7_make_symbol (sc, "parse-error"),
                   s7_list (sc, 1, s7_make_string_with_length (sc, msg.data (), (s7_int) msg.size ())));
}

// glue 时缓存 open-input-string 并永久 GC 保护：
// 避免每次调用都做全局查找，也避免被调用方环境中的同名绑定遮蔽
static s7_pointer cached_open_input_string= NULL;

// 与 (liii unicode) 的 codepoint->utf8 一致：不排斥代理区码点，逐字节编码
static bool
json_append_utf8 (std::string& out, s7_int cp) {
  if (cp < 0 || cp > 1114111) return false;
  if (cp <= 127) {
    out.push_back ((char) cp);
  }
  else if (cp <= 2047) {
    out.push_back ((char) (192 | ((cp >> 6) & 31)));
    out.push_back ((char) (128 | (cp & 63)));
  }
  else if (cp <= 65535) {
    out.push_back ((char) (224 | ((cp >> 12) & 15)));
    out.push_back ((char) (128 | ((cp >> 6) & 63)));
    out.push_back ((char) (128 | (cp & 63)));
  }
  else {
    out.push_back ((char) (240 | ((cp >> 18) & 7)));
    out.push_back ((char) (128 | ((cp >> 12) & 63)));
    out.push_back ((char) (128 | ((cp >> 6) & 63)));
    out.push_back ((char) (128 | (cp & 63)));
  }
  return true;
}

// 与 (string->number hex-str 16) 一致地解析 4 字符 hex（允许前导 +/-）
static bool
json_parse_hex4 (const char* s, s7_int n, s7_int& result) {
  s7_int i  = 0;
  bool   neg= false;
  if (i < n && (s[i] == '+' || s[i] == '-')) {
    neg= (s[i] == '-');
    i++;
  }
  if (i >= n) return false;
  s7_int v= 0;
  for (; i < n; i++) {
    char c= s[i];
    int  d;
    if (c >= '0' && c <= '9') d= c - '0';
    else if (c >= 'a' && c <= 'f') d= c - 'a' + 10;
    else if (c >= 'A' && c <= 'F') d= c - 'A' + 10;
    else return false;
    v= v * 16 + d;
  }
  result= neg ? -v : v;
  return true;
}

// [0125] 按 RFC 8259 重写的严格递归下降 parser，目标是 JSONTestSuite 的 y_/n_ 用例：
//   - 严格数字文法：-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?
//   - 严格结构文法：逗号/冒号缺失、尾逗号、尾部垃圾均报 parse-error
//   - 字符串内裸控制字符（< 0x20）报 parse-error
//   - 剥离 UTF-8 BOM；空白仅认 space/\t/\n/\r
//   - 顶层标量直接返回值；空/纯空白输入仍返回 eof-object（历史行为）
// 保留的 goldfish 扩展（与 RFC 的已知偏差）：
//   - 不带引号的对象键（{a:1}，键解析为符号）
// 单引号字符串不是合法 JSON，报 parse-error（与 0124 行为一致）
//
// 实现分两步：先解析为 C++ 侧的 jnode 树，再一次性转换为 s7 对象；
// 转换期间对每个已完成的子树用 s7_gc_protect_via_stack 保护，避免分配触发
// GC 回收未挂接的子树。

struct jnode {
  enum kind_t { JSTR, JSYM, JNUM, JTRUE, JFALSE, JNULL, JOBJ, JARR } kind;
  std::string             text;  // JSTR/JNUM: 字符串内容或数字文本；JSYM: 符号名
  std::vector<jnode>      items; // JARR
  std::vector<jnode>      keys;  // JOBJ（JSTR 或 JSYM）
  std::vector<jnode>      vals;  // JOBJ
};

struct json_parser {
  s7_scheme*  sc;
  const char* s;
  s7_int      len;
  s7_int      pos;
  s7_int      depth; // 当前容器嵌套深度
};

// 嵌套深度上限：防止恶意/损坏输入导致 C++ 递归爆栈（JSONTestSuite 最深合法用例为 500 层）
#define JSON_MAX_DEPTH 10000

static void
json_skip_ws (json_parser* p) {
  while (p->pos < p->len) {
    char c= p->s[p->pos];
    if (c == ' ' || c == '\t' || c == '\n' || c == '\r') p->pos++;
    else break;
  }
}

static s7_int
json_peek (json_parser* p) {
  return (p->pos < p->len) ? (unsigned char) p->s[p->pos] : -1;
}

static bool
json_parse_value (json_parser* p, jnode& out);

static bool
json_parse_hex4_at (json_parser* p, s7_int at, s7_int& cp) {
  if (at + 4 > p->len) return false;
  return json_parse_hex4 (p->s + at, 4, cp);
}

static bool
json_parse_string_body (json_parser* p, std::string& out) {
  // 进入时 p->pos 指向开引号之后
  while (p->pos < p->len) {
    unsigned char c= (unsigned char) p->s[p->pos];
    if (c == '"') {
      p->pos++;
      return true;
    }
    if (c == '\\') {
      if (p->pos + 1 >= p->len) return false;
      char next= p->s[p->pos + 1];
      switch (next) {
      case '"': out+= '"'; p->pos+= 2; break;
      case '\\': out+= '\\'; p->pos+= 2; break;
      case '/': out+= '/'; p->pos+= 2; break;
      case 'b': out+= '\b'; p->pos+= 2; break;
      case 'f': out+= '\f'; p->pos+= 2; break;
      case 'n': out+= '\n'; p->pos+= 2; break;
      case 'r': out+= '\r'; p->pos+= 2; break;
      case 't': out+= '\t'; p->pos+= 2; break;
      case 'u': {
        s7_int cp;
        if (!json_parse_hex4_at (p, p->pos + 2, cp)) return false;
        s7_int next_cp;
        if (cp >= 55296 && cp <= 56319 && p->pos + 6 + 6 < p->len
            && p->s[p->pos + 6] == '\\' && p->s[p->pos + 7] == 'u'
            && json_parse_hex4_at (p, p->pos + 8, next_cp)
            && next_cp >= 56320 && next_cp <= 57343) {
          cp= (cp - 55296) * 1024 + (next_cp - 56320) + 65536;
          p->pos+= 12;
        }
        else {
          p->pos+= 6;
        }
        if (!json_append_utf8 (out, cp)) return false;
        break;
      }
      default:
        return false;
      }
    }
    else if (c < 0x20) {
      return false; // 裸控制字符
    }
    else {
      out.push_back ((char) c);
      p->pos++;
    }
  }
  return false; // 未闭合
}

// 严格数字文法；返回 false 表示不是合法 JSON 数字
static bool
json_parse_number (json_parser* p, jnode& out) {
  s7_int bgn= p->pos;
  if (json_peek (p) == '-') p->pos++;
  s7_int c= json_peek (p);
  if (c == '0') {
    p->pos++;
  }
  else if (c >= '1' && c <= '9') {
    while (json_peek (p) >= '0' && json_peek (p) <= '9') p->pos++;
  }
  else return false;
  if (json_peek (p) == '.') {
    p->pos++;
    if (!(json_peek (p) >= '0' && json_peek (p) <= '9')) return false;
    while (json_peek (p) >= '0' && json_peek (p) <= '9') p->pos++;
  }
  c= json_peek (p);
  if (c == 'e' || c == 'E') {
    p->pos++;
    c= json_peek (p);
    if (c == '+' || c == '-') p->pos++;
    if (!(json_peek (p) >= '0' && json_peek (p) <= '9')) return false;
    while (json_peek (p) >= '0' && json_peek (p) <= '9') p->pos++;
  }
  out.kind= jnode::JNUM;
  out.text.assign (p->s + bgn, (size_t) (p->pos - bgn));
  return true;
}

// 不带引号的符号键：读到 ':'、空白或结构分隔符为止；至少一个字符
static bool
json_parse_symbol_key (json_parser* p, jnode& out) {
  s7_int bgn= p->pos;
  while (p->pos < p->len) {
    char c= p->s[p->pos];
    if (c == ':' || c == ',' || c == '}' || c == ']' || c == '[' || c == ' '
        || c == '\t' || c == '\n' || c == '\r' || c == '\'' || c == '"') break;
    p->pos++;
  }
  if (p->pos == bgn) return false;
  std::string text (p->s + bgn, (size_t) (p->pos - bgn));
  // [0125] 数字开头（含负号）的键是 JSON 数字而非符号；null/true/false 是保留字不能作键
  char c0= text[0];
  if ((c0 >= '0' && c0 <= '9') || c0 == '-') return false;
  if (text == "null" || text == "true" || text == "false") return false;
  out.kind= jnode::JSYM;
  out.text= std::move (text);
  return true;
}

static bool
json_parse_object (json_parser* p, jnode& out) {
  if (++p->depth > JSON_MAX_DEPTH) return false;
  p->pos++; // 跳过 '{'
  out.kind= jnode::JOBJ;
  json_skip_ws (p);
  if (json_peek (p) == '}') {
    p->pos++;
    p->depth--;
    return true;
  }
  while (true) {
    json_skip_ws (p);
    jnode key;
    if (json_peek (p) == '"') {
      p->pos++;
      key.kind= jnode::JSTR;
      if (!json_parse_string_body (p, key.text)) { p->depth--; return false; }
    }
    else if (!json_parse_symbol_key (p, key)) { p->depth--; return false; }
    json_skip_ws (p);
    if (json_peek (p) != ':') { p->depth--; return false; }
    p->pos++;
    json_skip_ws (p);
    jnode val;
    if (!json_parse_value (p, val)) { p->depth--; return false; }
    out.keys.push_back (std::move (key));
    out.vals.push_back (std::move (val));
    json_skip_ws (p);
    s7_int c= json_peek (p);
    if (c == ',') {
      p->pos++;
      continue; // 尾逗号会在下一轮的 key 解析处报错
    }
    p->depth--;
    if (c == '}') {
      p->pos++;
      return true;
    }
    return false;
  }
}

static bool
json_parse_array (json_parser* p, jnode& out) {
  if (++p->depth > JSON_MAX_DEPTH) return false;
  p->pos++; // 跳过 '['
  out.kind= jnode::JARR;
  json_skip_ws (p);
  if (json_peek (p) == ']') {
    p->pos++;
    p->depth--;
    return true;
  }
  while (true) {
    json_skip_ws (p);
    jnode val;
    if (!json_parse_value (p, val)) { p->depth--; return false; }
    out.items.push_back (std::move (val));
    json_skip_ws (p);
    s7_int c= json_peek (p);
    if (c == ',') {
      p->pos++;
      continue; // 尾逗号会在下一轮的 value 解析处报错
    }
    p->depth--;
    if (c == ']') {
      p->pos++;
      return true;
    }
    return false;
  }
}

static bool
json_parse_literal (json_parser* p, const char* lit) {
  size_t n= strlen (lit);
  if ((s7_int) (p->pos + n) > p->len) return false;
  if (strncmp (p->s + p->pos, lit, n) != 0) return false;
  p->pos+= (s7_int) n;
  return true;
}

static bool
json_parse_value (json_parser* p, jnode& out) {
  s7_int c= json_peek (p);
  switch (c) {
  case '{': return json_parse_object (p, out);
  case '[': return json_parse_array (p, out);
  case '"': {
    p->pos++;
    out.kind= jnode::JSTR;
    return json_parse_string_body (p, out.text);
  }
  case 't':
    if (!json_parse_literal (p, "true")) return false;
    out.kind= jnode::JTRUE;
    return true;
  case 'f':
    if (!json_parse_literal (p, "false")) return false;
    out.kind= jnode::JFALSE;
    return true;
  case 'n':
    if (!json_parse_literal (p, "null")) return false;
    out.kind= jnode::JNULL;
    return true;
  case '\'':
    // 单引号字符串不是合法 JSON（RFC 8259）
    return false;
  default:
    return json_parse_number (p, out);
  }
}

static bool
json_is_value_end (s7_int c) {
  return c == -1 || c == ',' || c == ']' || c == '}' || c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

// glue 时缓存 string->number 并永久 GC 保护：仅用于超出 int64 范围的大整数回退
static s7_pointer cached_string_to_number= NULL;

// 数字文本已在 parser 侧通过严格文法校验：
//   - 无 . e E 的整数走 strtoll + s7_make_integer（快路径）
//   - 其余走 strtod + s7_make_real（与 string->number 的双精度语义一致，1e2 => 100.0）
//   - int64 溢出时回退 cached string->number（保持 bignum 语义）
static s7_pointer
json_number_to_s7 (s7_scheme* sc, const std::string& text) {
  bool is_real= false;
  for (char c: text) {
    if (c == '.' || c == 'e' || c == 'E') {
      is_real= true;
      break;
    }
  }
  if (!is_real) {
    errno= 0;
    char* endp= NULL;
    long long v= strtoll (text.c_str (), &endp, 10);
    if (errno != ERANGE && endp && *endp == '\0') {
      return s7_make_integer (sc, (s7_int) v);
    }
  }
  else {
    errno= 0;
    char* endp= NULL;
    double d= strtod (text.c_str (), &endp);
    if (endp && *endp == '\0') {
      return s7_make_real (sc, d);
    }
  }
  // 回退：超大整数等罕见情形
  s7_pointer txt= s7_make_string_with_length (sc, text.data (), (s7_int) text.size ());
  s7_gc_protect_via_stack (sc, txt);
  s7_pointer num= s7_call (sc, cached_string_to_number, s7_list (sc, 1, txt));
  s7_gc_unprotect_via_stack (sc, txt);
  return num;
}

// 调用方（f_string_to_json）已在转换期间关闭 GC（参考 njson 的做法），
// 故此处无需对中间对象做逐节点保护
static s7_pointer
json_node_to_s7 (s7_scheme* sc, const jnode& n) {
  switch (n.kind) {
  case jnode::JSTR:
    return s7_make_string_with_length (sc, n.text.data (), (s7_int) n.text.size ());
  case jnode::JSYM:
    return s7_make_symbol (sc, n.text.c_str ());
  case jnode::JNUM:
    return json_number_to_s7 (sc, n.text);
  case jnode::JTRUE: return s7_make_symbol (sc, "true");
  case jnode::JFALSE: return s7_make_symbol (sc, "false");
  case jnode::JNULL: return s7_make_symbol (sc, "null");
  case jnode::JOBJ: {
    if (n.keys.empty ()) {
      // 空对象 '()
      return s7_cons (sc, s7_nil (sc), s7_nil (sc));
    }
    // 自底向顶构建 alist，最后整体 reverse
    s7_pointer lst= s7_nil (sc);
    for (size_t i= 0; i < n.keys.size (); i++) {
      s7_pointer key= json_node_to_s7 (sc, n.keys[i]);
      s7_pointer val= json_node_to_s7 (sc, n.vals[i]);
      lst= s7_cons (sc, s7_cons (sc, key, val), lst);
    }
    return s7_reverse (sc, lst);
  }
  case jnode::JARR: {
    s7_pointer vec= s7_make_vector (sc, (s7_int) n.items.size ());
    s7_pointer* elems= s7_vector_elements (vec);
    for (size_t i= 0; i < n.items.size (); i++) {
      elems[i]= json_node_to_s7 (sc, n.items[i]);
    }
    return vec;
  }
  }
  return s7_nil (sc);
}

static s7_pointer
f_string_to_json (s7_scheme* sc, s7_pointer args) {
  s7_pointer arg= s7_car (args);
  if (!s7_is_string (arg)) {
    return s7_wrong_type_arg_error (sc, "string->json", 1, arg, "a string");
  }
  const char* s  = s7_string (arg);
  s7_int      len= s7_string_length (arg);

  json_parser p= {sc, s, len, 0, 0};
  // 剥离 UTF-8 BOM
  if (len >= 3 && (unsigned char) s[0] == 0xEF && (unsigned char) s[1] == 0xBB && (unsigned char) s[2] == 0xBF)
    p.pos= 3;
  json_skip_ws (&p);
  if (json_peek (&p) == -1) {
    // 空输入/纯空白输入：保持历史行为返回 eof-object
    return s7_eof_object (sc);
  }
  jnode root;
  if (!json_parse_value (&p, root) || !json_is_value_end (json_peek (&p))) {
    return json_parse_error (sc, "string->json: invalid JSON");
  }
  json_skip_ws (&p);
  if (json_peek (&p) != -1) {
    return json_parse_error (sc, "string->json: trailing garbage after JSON value");
  }
  // 转换期间关闭 GC（参考 njson 的做法）：jnode 树已在 C++ 侧完整构造，
  // 转换过程不会中断，无需逐节点保护；结束后恢复
  s7_gc_on (sc, false);
  s7_pointer result= json_node_to_s7 (sc, root);
  s7_gc_on (sc, true);
  return result;
}

static void
glue_string_to_json (s7_scheme* sc) {
  const char* name= "g_string->json";
  const char* desc= "(g_string->json str) => data, parse a JSON string to Scheme-form JSON data";
  s7_define_function (sc, name, f_string_to_json, 1, 0, false, desc);
  cached_open_input_string= s7_name_to_value (sc, "open-input-string");
  s7_gc_protect (sc, cached_open_input_string);
  cached_string_to_number= s7_name_to_value (sc, "string->number");
  s7_gc_protect (sc, cached_string_to_number);
}


// json-ref / json-set 的 C++ 实现，语义与历史上 (liii json) 包装 (guenchi json)
// 的 Scheme 实现完全一致：
//   - json-ref：多键路径逐层下钻；'() 透传（安全导航）；空对象 '(()) 读出为 '()；
//     每层先做结构校验（非对象/数组抛 type-error）；读到的符号 'true/'false 转为 #t/#f；
//     数组用 vector-ref 语义（非整数索引抛 wrong-type-arg、越界抛 out-of-range）
//   - json-set：多键路径逐层函数式更新；空对象 '(()) 原样返回；
//     键 #t 表示映射所有值；键为过程表示按键谓词筛选；普通键按 equal? 匹配；
//     匹配成功后新序对的键：普通键分支用传入的键，#t/过程键分支保留原键；
//     叶层值可以是过程（接收旧值返回新值）；
//     键 #f 落入 guenchi (if v ...) 无 else 分支的历史怪癖原样保留
//     （对象返回 #<unspecified>，数组走 (list->vector #<unspecified>) 抛 wrong-type-arg）

// 真列表长度；非真列表（含循环列表）返回 -1
static s7_int
json_proper_list_length (s7_scheme* sc, s7_pointer x) {
  s7_pointer slow= x, fast= x;
  s7_int     len= 0;
  while (s7_is_pair (fast)) {
    fast= s7_cdr (fast);
    len++;
    if (s7_is_pair (fast)) {
      fast= s7_cdr (fast);
      len++;
      slow= s7_cdr (slow);
      if (fast == slow) return -1; // 循环列表
    }
  }
  if (!s7_is_null (sc, fast)) return -1;
  return len;
}

// 空对象 '(())
static bool
json_is_null_object (s7_scheme* sc, s7_pointer x) {
  return s7_is_pair (x) && s7_is_null (sc, s7_car (x)) && s7_is_null (sc, s7_cdr (x));
}

// 与 (liii json) 的 json-object? 一致（x 已知非 '()）：
// (and (list? x) (not (null? x)) (or (equal? x '(())) (every pair? x)))
static bool
json_is_object (s7_scheme* sc, s7_pointer x, s7_int& len) {
  if (!s7_is_pair (x)) return false;
  if (json_is_null_object (sc, x)) {
    len= 1;
    return true;
  }
  len= json_proper_list_length (sc, x);
  if (len < 0) return false;
  s7_pointer p= x;
  while (s7_is_pair (p)) {
    if (!s7_is_pair (s7_car (p))) return false;
    p= s7_cdr (p);
  }
  return true;
}

// glue 时缓存 vector-ref / list->vector 并永久 GC 保护：
// 仅在报错路径上调用，保证错误类型和消息与 Scheme 实现逐字节一致
static s7_pointer cached_vector_ref    = NULL;
static s7_pointer cached_list_to_vector= NULL;

// guenchi json-ref 的 return 包装：'true -> #t，'false -> #f
static s7_pointer symbol_true = NULL;
static s7_pointer symbol_false= NULL;

static s7_pointer
json_ref_convert (s7_scheme* sc, s7_pointer x) {
  if (s7_is_symbol (x)) {
    if (x == symbol_true) return s7_t (sc);
    if (x == symbol_false) return s7_f (sc);
  }
  return x;
}

static s7_pointer
f_json_ref (s7_scheme* sc, s7_pointer args) {
  s7_pointer cur = s7_car (args);
  s7_pointer keys= s7_cdr (args);
  while (s7_is_pair (keys)) {
    s7_pointer key= s7_car (keys);
    keys          = s7_cdr (keys);
    // '() 透传：安全导航，直接返回 '()
    if (s7_is_null (sc, cur)) return s7_nil (sc);
    s7_pointer val;
    if (s7_is_vector (cur)) {
      if (s7_is_integer (key)) {
        s7_int i= s7_integer (key);
        if (i >= 0 && i < s7_vector_length (cur)) {
          val= s7_vector_elements (cur)[i];
        }
        else {
          // 抛出与 vector-ref 一致的 out-of-range 错误
          return s7_call (sc, cached_vector_ref, s7_list (sc, 2, cur, key));
        }
      }
      else {
        // 抛出与 vector-ref 一致的 wrong-type-arg 错误
        return s7_call (sc, cached_vector_ref, s7_list (sc, 2, cur, key));
      }
    }
    else if (s7_is_pair (cur)) {
      if (json_is_null_object (sc, cur)) {
        val= s7_nil (sc);
      }
      else {
        s7_int len;
        if (!json_is_object (sc, cur, len)) {
          return json_type_error (sc, "Value is not a JSON object or array", cur);
        }
        val         = s7_nil (sc);
        s7_pointer p= cur;
        while (s7_is_pair (p)) {
          s7_pointer entry= s7_car (p);
          if (s7_is_equal (sc, s7_car (entry), key)) {
            val= s7_cdr (entry);
            break;
          }
          p= s7_cdr (p);
        }
      }
    }
    else {
      return json_type_error (sc, "Value is not a JSON object or array", cur);
    }
    cur= json_ref_convert (sc, val);
  }
  return cur;
}

static void
glue_json_ref (s7_scheme* sc) {
  const char* name= "g_json_ref";
  const char* desc= "(g_json_ref json key . keys) => value, ref a value from Scheme-form JSON data by key path";
  s7_define_function (sc, name, f_json_ref, 2, 0, true, desc);
  cached_vector_ref= s7_name_to_value (sc, "vector-ref");
  s7_gc_protect (sc, cached_vector_ref);
  symbol_true= s7_make_symbol (sc, "true");
  s7_gc_protect (sc, symbol_true);
  symbol_false= s7_make_symbol (sc, "false");
  s7_gc_protect (sc, symbol_false);
}

// json-set 的叶层写入器：rest 非空表示多键路径（对旧值递归 json-set），
// 否则写入叶层值（叶层值为过程时以旧值调用之）
struct json_setter {
  s7_pointer rest; // 剩余 (key val ...) 参数；'() 表示已到叶层
  s7_pointer leaf; // 叶层值（仅 rest 为 '() 时有效）
  bool       leaf_is_proc;
};

static s7_pointer json_set_dispatch (s7_scheme* sc, s7_pointer x, s7_pointer kargs);

static s7_pointer
json_setter_apply (s7_scheme* sc, const json_setter& st, s7_pointer old) {
  if (!s7_is_null (sc, st.rest)) return json_set_dispatch (sc, old, st.rest);
  if (st.leaf_is_proc) return s7_call (sc, st.leaf, s7_list (sc, 1, old));
  return st.leaf;
}

// guenchi json-set 的单层语义：x 已校验为 JSON 对象（含 len 个条目）或数组
static s7_pointer
json_guenchi_set (s7_scheme* sc, s7_pointer x, s7_pointer v, s7_int len, const json_setter& st) {
  if (s7_is_vector (x)) {
    s7_int      n    = s7_vector_length (x);
    s7_pointer* elems= s7_vector_elements (x);
    if (s7_is_boolean (v) && !s7_boolean (sc, v)) {
      // guenchi 的 (if v ...) 无 else 分支：(list->vector #<unspecified>) 抛 wrong-type-arg
      return s7_call (sc, cached_list_to_vector, s7_list (sc, 1, s7_unspecified (sc)));
    }
    s7_pointer result= s7_make_vector (sc, n);
    s7_gc_protect_via_stack (sc, result);
    s7_pointer* relems= s7_vector_elements (result);
    for (s7_int i= 0; i < n; i++) {
      bool replace;
      if (s7_is_boolean (v)) replace= true;
      else if (s7_is_procedure (v)) {
        replace= (s7_call (sc, v, s7_list (sc, 1, s7_make_integer (sc, i))) != s7_f (sc));
      }
      else {
        replace= s7_is_equal (sc, s7_make_integer (sc, i), v);
      }
      relems[i]= replace ? json_setter_apply (sc, st, elems[i]) : elems[i];
    }
    s7_gc_unprotect_via_stack (sc, result);
    return result;
  }
  // 对象（alist）：键 #t 或过程键保留原键，普通键匹配后用传入的键构造新序对
  bool map_all= false, use_pred= false;
  if (s7_is_boolean (v)) {
    if (!s7_boolean (sc, v)) return s7_unspecified (sc); // (if v ...) 无 else 分支
    map_all= true;
  }
  else if (s7_is_procedure (v)) {
    use_pred= true;
  }
  // 先搭建与输入等长的结果骨架并 GC 保护（搭建期间不调用用户过程）
  s7_pointer head= s7_cons (sc, s7_nil (sc), s7_nil (sc));
  s7_gc_protect_via_stack (sc, head);
  s7_pointer tail= head;
  for (s7_int i= 1; i < len; i++) {
    s7_set_cdr (tail, s7_cons (sc, s7_nil (sc), s7_nil (sc)));
    tail= s7_cdr (tail);
  }
  s7_pointer p= x;
  tail        = head;
  while (s7_is_pair (p)) {
    s7_pointer entry= s7_car (p);
    bool       replace;
    if (map_all) replace= true;
    else if (use_pred) {
      replace= (s7_call (sc, v, s7_list (sc, 1, s7_car (entry))) != s7_f (sc));
    }
    else {
      replace= s7_is_equal (sc, s7_car (entry), v);
    }
    if (replace) {
      s7_pointer newkey= (map_all || use_pred) ? s7_car (entry) : v;
      s7_set_car (tail, s7_cons (sc, newkey, json_setter_apply (sc, st, s7_cdr (entry))));
    }
    else {
      s7_set_car (tail, entry); // 未匹配的条目复用原序对
    }
    tail= s7_cdr (tail);
    p   = s7_cdr (p);
  }
  s7_gc_unprotect_via_stack (sc, head);
  return head;
}

// 对应 (liii json) 的 json-set 包装：结构校验 + '(()) 特判 + 单键/多键分派
// kargs 为 (key val . rest)
static s7_pointer
json_set_dispatch (s7_scheme* sc, s7_pointer x, s7_pointer kargs) {
  s7_int len= 0;
  if (!s7_is_vector (x) && !json_is_object (sc, x, len)) {
    return json_type_error (sc, "Value is not a JSON object or array", x);
  }
  // 空对象 '(()) 原样返回（单键与多键均如此）
  if (json_is_null_object (sc, x)) return x;
  s7_pointer  key = s7_car (kargs);
  s7_pointer  rest= s7_cdr (kargs);
  json_setter st;
  if (s7_is_null (sc, s7_cdr (rest))) {
    st.rest        = s7_nil (sc);
    st.leaf        = s7_car (rest);
    st.leaf_is_proc= s7_is_procedure (st.leaf);
  }
  else {
    st.rest        = rest;
    st.leaf        = NULL;
    st.leaf_is_proc= false;
  }
  return json_guenchi_set (sc, x, key, len, st);
}

static s7_pointer
f_json_set (s7_scheme* sc, s7_pointer args) {
  return json_set_dispatch (sc, s7_car (args), s7_cdr (args));
}

static void
glue_json_set (s7_scheme* sc) {
  const char* name= "g_json_set";
  const char* desc=
      "(g_json_set json key val . keys-and-val) => data, set a value in Scheme-form JSON data by key path";
  s7_define_function (sc, name, f_json_set, 3, 0, true, desc);
  cached_list_to_vector= s7_name_to_value (sc, "list->vector");
  s7_gc_protect (sc, cached_list_to_vector);
}

void
glue_liii_json (s7_scheme* sc) {
  glue_json_to_string (sc);
  glue_string_to_json (sc);
  glue_json_ref (sc);
  glue_json_set (sc);
}

} // namespace goldfish
