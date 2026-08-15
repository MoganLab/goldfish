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
        out.push_back ((char) c);
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

static s7_pointer
f_string_to_json (s7_scheme* sc, s7_pointer args) {
  s7_pointer arg= s7_car (args);
  if (!s7_is_string (arg)) {
    return s7_wrong_type_arg_error (sc, "string->json", 1, arg, "a string");
  }
  const char* s  = s7_string (arg);
  s7_int      len= s7_string_length (arg);

  std::string out;
  out.reserve ((size_t) len + 8);
  s7_int bgn = 0;
  s7_int end = 0;
  bool   quts= false;
  // 上下文栈：栈顶为真表示对象（"," 改写为 ")("），为假表示数组（"," 改写为 " "）。
  // 空栈按真处理（对应 Scheme 版 (loose-car '()) => '() 为真值）。
  std::string stk;
  stk.push_back (1);

  while (end < len) {
    char c= s[end];
    if (quts && c == '\\' && end + 1 < len) {
      char next= s[end + 1];
      switch (next) {
      case '"':
      case '\\':
      case 'b':
      case 'f':
      case 'n':
      case 'r':
      case 't':
        // 转义序列原样保留，交由 reader 处理
        out.append (s + bgn, (size_t) (end + 2 - bgn));
        bgn= end= end + 2;
        break;
      case '/':
        out.append (s + bgn, (size_t) (end - bgn));
        out.push_back ('/');
        bgn= end= end + 2;
        break;
      case 'u': {
        s7_int start_pos= end + 2;
        s7_int end_pos  = end + 6;
        if (!(end_pos < len)) {
          std::string msg= "HEX sequence too short ";
          msg.append (s + start_pos, (size_t) (len - start_pos));
          return json_parse_error (sc, msg);
        }
        s7_int cp;
        if (!json_parse_hex4 (s + start_pos, 4, cp)) {
          std::string msg= "Invalid HEX sequence ";
          msg.append (s + start_pos, 4);
          return json_parse_error (sc, msg);
        }
        s7_int next_u_pos= end + 6;
        if (next_u_pos + 6 < len && s[next_u_pos] == '\\' && s[next_u_pos + 1] == 'u') {
          s7_int next_cp;
          if (!json_parse_hex4 (s + next_u_pos + 2, 4, next_cp)) {
            std::string msg= "Invalid HEX sequence ";
            msg.append (s + next_u_pos + 2, 4);
            return json_parse_error (sc, msg);
          }
          if (cp >= 55296 && cp <= 56319 && next_cp >= 56320 && next_cp <= 57343) {
            s7_int combined= (cp - 55296) * 1024 + (next_cp - 56320) + 65536;
            out.append (s + bgn, (size_t) (end - bgn));
            if (!json_append_utf8 (out, combined)) {
              return s7_error (sc, s7_make_symbol (sc, "value-error"),
                               s7_list (sc, 2, s7_make_string (sc, "codepoint->utf8: codepoint out of Unicode range"),
                                        s7_make_integer (sc, combined)));
            }
            bgn= end= end + 12;
            break;
          }
        }
        out.append (s + bgn, (size_t) (end - bgn));
        if (!json_append_utf8 (out, cp)) {
          return s7_error (sc, s7_make_symbol (sc, "value-error"),
                           s7_list (sc, 2, s7_make_string (sc, "codepoint->utf8: codepoint out of Unicode range"),
                                    s7_make_integer (sc, cp)));
        }
        bgn= end= end + 6;
        break;
      }
      default: {
        std::string msg= "Invalid escape char: ";
        msg.push_back (next);
        return json_parse_error (sc, msg);
      }
      }
    }
    else if (quts && c != '"') {
      end++;
    }
    else {
      switch (c) {
      case '{':
        out.append (s + bgn, (size_t) (end - bgn));
        out+= "((";
        bgn= end= end + 1;
        stk.push_back (1);
        break;
      case '}':
        out.append (s + bgn, (size_t) (end - bgn));
        out+= "))";
        bgn= end= end + 1;
        if (!stk.empty ()) stk.pop_back ();
        break;
      case '[':
        out.append (s + bgn, (size_t) (end - bgn));
        out+= "#(";
        bgn= end= end + 1;
        stk.push_back (0);
        break;
      case ']':
        out.append (s + bgn, (size_t) (end - bgn));
        out.push_back (')');
        bgn= end= end + 1;
        if (!stk.empty ()) stk.pop_back ();
        break;
      case ':':
        out.append (s + bgn, (size_t) (end - bgn));
        out+= " . ";
        bgn= end= end + 1;
        break;
      case ',':
        out.append (s + bgn, (size_t) (end - bgn));
        out+= (stk.empty () || stk.back ()) ? ")(" : " ";
        bgn= end= end + 1;
        break;
      case '"':
        quts= !quts;
        end++;
        break;
      default:
        end++;
        break;
      }
    }
  }
  // 与 Scheme 版一致：最后一个分隔符之后的内容不落盘

  // 改写结果可能含 NUL 字节（来自 \u0000），故不能用基于 C 字符串的
  // s7_open_input_string；构造定长 Scheme 字符串后走与原来一致的 read 流程。
  // 注意：data_str 一旦创建就要立即 GC 保护——s7_list 和 open-input-string
  // 都会分配内存，若其间触发 GC，未保护的 data_str 会被回收，
  // port 将指向已释放的内存（曾导致 Windows CI 偶发 0xC0000005）
  s7_pointer data_str= s7_make_string_with_length (sc, out.data (), (s7_int) out.size ());
  s7_gc_protect_via_stack (sc, data_str);
  s7_pointer port= s7_call (sc, cached_open_input_string, s7_list (sc, 1, data_str));
  s7_gc_protect_via_stack (sc, port);
  s7_pointer result= s7_read (sc, port);
  s7_gc_unprotect_via_stack (sc, port);
  s7_gc_unprotect_via_stack (sc, data_str);
  return result;
}

static void
glue_string_to_json (s7_scheme* sc) {
  const char* name= "g_string->json";
  const char* desc= "(g_string->json str) => data, parse a JSON string to Scheme-form JSON data";
  s7_define_function (sc, name, f_string_to_json, 1, 0, false, desc);
  cached_open_input_string= s7_name_to_value (sc, "open-input-string");
  s7_gc_protect (sc, cached_open_input_string);
}

void
glue_liii_json (s7_scheme* sc) {
  glue_json_to_string (sc);
  glue_string_to_json (sc);
}

} // namespace goldfish
