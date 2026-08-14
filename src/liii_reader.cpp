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
#include <string>

namespace goldfish {

// A tiny, non-R7RS reader used only to bootstrap the Scheme reader
// (goldfish/liii/reader.scm). It understands a small subset:
//   lists (with dotted pairs), quote abbreviation, strings, integers,
//   symbols, #t/#f, #\ characters, ; line comments.
// It deliberately does NOT handle vectors, #u8, |...| symbols, datum
// labels, #; datum comments, #! directives, etc.

static bool tiny_is_ws (s7_int c) {
  return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f';
}

static s7_int tiny_peek (s7_scheme* sc, s7_pointer port) {
  s7_pointer p = s7_peek_char (sc, port);
  return s7_is_character (p) ? (s7_int) s7_character (p) : -1;
}

static s7_int tiny_next (s7_scheme* sc, s7_pointer port) {
  s7_pointer p = s7_read_char (sc, port);
  return s7_is_character (p) ? (s7_int) s7_character (p) : -1;
}

static bool tiny_is_delim (s7_int c) {
  return c < 0 || tiny_is_ws (c) || c == '(' || c == ')' || c == '\'' ||
         c == '"' || c == ';' || c == '[' || c == ']';
}

static void tiny_skip_ws (s7_scheme* sc, s7_pointer port) {
  while (true) {
    s7_int c = tiny_peek (sc, port);
    if (c >= 0 && tiny_is_ws (c)) {
      tiny_next (sc, port);
    } else if (c == ';') {
      while (true) {
        s7_int d = tiny_next (sc, port);
        if (d < 0 || d == '\n')
          break;
      }
    } else {
      return;
    }
  }
}

static s7_pointer tiny_read_form (s7_scheme* sc, s7_pointer port);

static s7_pointer tiny_read_string_core (s7_scheme* sc, s7_pointer port, s7_int rdelim);

static s7_pointer
tiny_read_string (s7_scheme* sc, s7_pointer port) {
  tiny_next (sc, port);  // consume "
  return tiny_read_string_core (sc, port, '"');
}

static s7_pointer
tiny_read_char (s7_scheme* sc, s7_pointer port) {
  s7_int c = tiny_next (sc, port);
  if (c < 0) {
    return s7_error (sc, s7_make_symbol (sc, "read-error"),
                     s7_list (sc, 1, s7_make_string (sc, "unexpected end of input in character")));
  }
  // hex escape: #\x followed by hex digits (or #\x alone = the char x)
  if (c == 'x' || c == 'X') {
    s7_int h = tiny_peek (sc, port);
    int d;
    if (h >= '0' && h <= '9') d = h - '0';
    else if (h >= 'a' && h <= 'f') d = h - 'a' + 10;
    else if (h >= 'A' && h <= 'F') d = h - 'A' + 10;
    else return s7_make_character (sc, c);  // plain #\x
    int v = 0;
    while (true) {
      s7_int hh = tiny_peek (sc, port);
      int dd;
      if (hh >= '0' && hh <= '9') dd = hh - '0';
      else if (hh >= 'a' && hh <= 'f') dd = hh - 'a' + 10;
      else if (hh >= 'A' && hh <= 'F') dd = hh - 'A' + 10;
      else break;
      tiny_next (sc, port);
      v = v * 16 + dd;
    }
    return s7_make_character (sc, v);
  }
  // named characters
  static const char* names[] = {
    "alarm", "backspace", "delete", "escape", "newline", "null",
    "return", "space", "tab"
  };
  static const int values[] = {7, 8, 127, 27, 10, 0, 13, 32, 9};
  if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
    std::string tok;
    tok += (char) c;
    while (!tiny_is_delim (tiny_peek (sc, port)))
      tok += (char) tiny_next (sc, port);
    for (size_t i = 0; i < 9; i++) {
      if (tok == names[i])
        return s7_make_character (sc, values[i]);
    }
    if (tok.size () == 1)
      return s7_make_character (sc, (int) tok[0]);
    return s7_error (sc, s7_make_symbol (sc, "read-error"),
                     s7_list (sc, 1, s7_make_string (sc, "invalid character")));
  }
  return s7_make_character (sc, c);
}

static s7_pointer
tiny_read_token (s7_scheme* sc, s7_pointer port, s7_int first) {
  std::string tok;
  tok += (char) first;
  while (!tiny_is_delim (tiny_peek (sc, port)))
    tok += (char) tiny_next (sc, port);
  // integer?
  bool neg = false;
  size_t i = 0;
  if (i < tok.size () && (tok[i] == '+' || tok[i] == '-')) {
    neg = (tok[i] == '-');
    i++;
  }
  bool all_digits = (i < tok.size ());
  for (size_t k = i; k < tok.size (); k++) {
    if (tok[k] < '0' || tok[k] > '9') {
      all_digits = false;
      break;
    }
  }
  if (all_digits) {
    long long v = 0;
    for (size_t k = i; k < tok.size (); k++)
      v = v * 10 + (tok[k] - '0');
    if (neg)
      v = -v;
    return s7_make_integer (sc, (s7_int) v);
  }
  return s7_make_symbol (sc, tok.c_str ());
}

// The Scheme reader's delimiter set (goldfish/liii/reader.scm `delimiter?'
// delegates to g-delimiter?): whitespace, ( ) [ ] " ; .  Note that ' and `
// are NOT delimiters there (unlike tiny_is_delim, which is only used for the
// bootstrap reader).  Single source of truth: g-read-token uses the same set.
static bool scheme_delim (s7_int c) {
  return c < 0 || c == '(' || c == ')' || c == '[' || c == ']' ||
         c == ';' || c == '"' || c == ' ' || c == '\t' || c == '\n' ||
         c == '\r' || c == '\f' || c == '\xc';
}

static s7_pointer
f_g_delimiter_p (s7_scheme* sc, s7_pointer args) {
  s7_pointer ch = s7_car (args);
  if (!s7_is_character (ch))
    return s7_f (sc);
  return scheme_delim ((s7_int) s7_character (ch)) ? s7_t (sc) : s7_f (sc);
}

// g-read-token : port first-char -> string
// Read one token (first-char already consumed) up to the Scheme reader's
// delimiter set, returning the raw token text.  The Scheme reader does the
// interpretation (number vs symbol, case folding), so C++ stays a thin,
// fast character pump for the hottest parsing path.
static s7_pointer
f_g_read_token (s7_scheme* sc, s7_pointer args) {
  s7_pointer port = s7_car (args);
  s7_pointer first_p = s7_cadr (args);
  s7_int     first = s7_is_character (first_p) ? (s7_int) s7_character (first_p)
                                               : (s7_int) first_p;
  std::string tok;
  tok += (char) first;
  while (true) {
    s7_int c = tiny_peek (sc, port);
    if (scheme_delim (c))
      break;
    tok += (char) tiny_next (sc, port);
  }
  return s7_make_string (sc, tok.c_str ());
}

// g-read-string : port [rdelim] -> string
// Read a quoted string (the opening rdelim, normally ", already consumed),
// mirroring the Scheme reader's read-quoted-string: intraline line-ending
// continuations, the R7RS + S7 escape set, and \xHH; as UTF-8.
static void
g_append_utf8 (std::string& s, int v) {
  if (v <= 0x7f) {
    s += (char) v;
  } else if (v <= 0x7ff) {
    s += (char) (0xc0 | (v >> 6));
    s += (char) (0x80 | (v & 0x3f));
  } else if (v <= 0xffff) {
    s += (char) (0xe0 | (v >> 12));
    s += (char) (0x80 | ((v >> 6) & 0x3f));
    s += (char) (0x80 | (v & 0x3f));
  } else {
    s += (char) (0xf0 | (v >> 18));
    s += (char) (0x80 | ((v >> 12) & 0x3f));
    s += (char) (0x80 | ((v >> 6) & 0x3f));
    s += (char) (0x80 | (v & 0x3f));
  }
}

static s7_pointer
tiny_read_string_core (s7_scheme* sc, s7_pointer port, s7_int rdelim) {
  std::string s;
  while (true) {
    s7_int c = tiny_next (sc, port);
    if (c < 0) {
      return s7_error (sc, s7_make_symbol (sc, "read-error"),
                       s7_list (sc, 1, s7_make_string (sc, "unterminated string")));
    }
    if (c == rdelim)
      break;
    if (c != '\\') {
      s += (char) c;
      continue;
    }
    s7_int e = tiny_next (sc, port);
    if (e < 0) {
      return s7_error (sc, s7_make_symbol (sc, "read-error"),
                       s7_list (sc, 1, s7_make_string (sc, "unterminated string")));
    }
    if (e == '\n' || e == '\r') {
      // line continuation: consume the whole line ending, then intraline ws
      if (e == '\r' && tiny_peek (sc, port) == '\n')
        tiny_next (sc, port);
      while (true) {
        s7_int p = tiny_peek (sc, port);
        if (p == ' ' || p == '\t')
          tiny_next (sc, port);
        else
          break;
      }
      continue;
    }
    if (e == rdelim) {
      s += (char) rdelim;
      continue;
    }
    switch (e) {
      case 'a':  s += '\a'; break;
      case 'b':  s += '\b'; break;
      case 't':  s += '\t'; break;
      case 'n':  s += '\n'; break;
      case 'r':  s += '\r'; break;
      case 'f':  s += '\f'; break;
      case 'v':  s += '\v'; break;
      case '0':  s += '\0'; break;
      case 'e':  s += (char) 27; break;
      case '\\': s += '\\'; break;
      case '"':  s += '"'; break;
      case '|':  s += '|'; break;
      case 'x': {
        int v = 0;
        int nd = 0;
        while (true) {
          s7_int h = tiny_peek (sc, port);
          int d;
          if (h >= '0' && h <= '9') d = h - '0';
          else if (h >= 'a' && h <= 'f') d = h - 'a' + 10;
          else if (h >= 'A' && h <= 'F') d = h - 'A' + 10;
          else break;
          tiny_next (sc, port);
          v = v * 16 + d;
          nd++;
        }
        if (nd == 0) {
          return s7_error (sc, s7_make_symbol (sc, "read-error"),
                           s7_list (sc, 1, s7_make_string (sc, "invalid hex escape")));
        }
        if (tiny_peek (sc, port) != ';') {
          return s7_error (sc, s7_make_symbol (sc, "read-error"),
                           s7_list (sc, 1, s7_make_string (sc, "hex escape missing semicolon")));
        }
        tiny_next (sc, port);
        g_append_utf8 (s, v);
        break;
      }
      default:
        return s7_error (sc, s7_make_symbol (sc, "read-error"),
                         s7_list (sc, 1, s7_make_string (sc, "invalid character in escape sequence")));
    }
  }
  return s7_make_string_with_length (sc, s.c_str (), (s7_int) s.size ());
}

static s7_pointer
f_g_read_string (s7_scheme* sc, s7_pointer args) {
  s7_pointer port = s7_car (args);
  s7_int     rdelim = '"';
  if (s7_is_character (s7_cadr (args)))
    rdelim = (s7_int) s7_character (s7_cadr (args));
  return tiny_read_string_core (sc, port, rdelim);
}

static s7_pointer
tiny_read_form (s7_scheme* sc, s7_pointer port) {
  tiny_skip_ws (sc, port);
  s7_int c = tiny_peek (sc, port);
  if (c < 0)
    return s7_eof_object (sc);
  if (c == '(' || c == '[') {
    const int close = (c == '(') ? ')' : ']';
    tiny_next (sc, port);
    s7_pointer head = s7_nil (sc);
    s7_pointer tail = s7_nil (sc);
    bool first = true;
    while (true) {
      tiny_skip_ws (sc, port);
      s7_int d = tiny_peek (sc, port);
      if (d < 0) {
        return s7_error (sc, s7_make_symbol (sc, "read-error"),
                         s7_list (sc, 1, s7_make_string (sc, "unterminated list")));
      }
      if (d == close) {
        tiny_next (sc, port);
        return head;
      }
      if (d == '.' && (first || true)) {
        // dotted pair: require a delimiter after the dot
        s7_int after = tiny_peek (sc, port);
        (void) after;
        // peek past '.' without consuming: the dot is standalone iff the
        // following char is a delimiter
        tiny_next (sc, port);
        if (tiny_is_delim (tiny_peek (sc, port))) {
          if (first) {
            return s7_error (sc, s7_make_symbol (sc, "read-error"),
                             s7_list (sc, 1, s7_make_string (sc, "dot with no element")));
          }
          s7_pointer b = tiny_read_form (sc, port);
          tiny_skip_ws (sc, port);
          if (tiny_peek (sc, port) != close) {
            return s7_error (sc, s7_make_symbol (sc, "read-error"),
                             s7_list (sc, 1, s7_make_string (sc, "bad dotted pair")));
          }
          tiny_next (sc, port);
          s7_set_cdr (tail, b);
          return head;
        }
        // not a standalone dot: fall through and read it as a token
        return tiny_read_token (sc, port, '.');
      }
      s7_pointer el = tiny_read_form (sc, port);
      if (first) {
        head = s7_cons (sc, el, s7_nil (sc));
        tail = head;
        first = false;
      } else {
        s7_pointer cell = s7_cons (sc, el, s7_nil (sc));
        s7_set_cdr (tail, cell);
        tail = cell;
      }
    }
  }
  if (c == ')' || c == ']') {
    tiny_next (sc, port);
    return s7_error (sc, s7_make_symbol (sc, "read-error"),
                     s7_list (sc, 1, s7_make_string (sc, "unexpected )")));
  }
  if (c == '\'') {
    tiny_next (sc, port);
    return s7_cons (sc, s7_make_symbol (sc, "quote"),
                    s7_cons (sc, tiny_read_form (sc, port), s7_nil (sc)));
  }
  if (c == '`') {
    tiny_next (sc, port);
    return s7_cons (sc, s7_make_symbol (sc, "quasiquote"),
                    s7_cons (sc, tiny_read_form (sc, port), s7_nil (sc)));
  }
  if (c == ',') {
    tiny_next (sc, port);
    const char* name = "unquote";
    if (tiny_peek (sc, port) == '@') {
      tiny_next (sc, port);
      name = "unquote-splicing";
    }
    return s7_cons (sc, s7_make_symbol (sc, name),
                    s7_cons (sc, tiny_read_form (sc, port), s7_nil (sc)));
  }
  if (c == '"')
    return tiny_read_string (sc, port);
  if (c == '#') {
    tiny_next (sc, port);
    s7_int d = tiny_peek (sc, port);
    if (d == 't') { tiny_next (sc, port); return s7_t (sc); }
    if (d == 'f') { tiny_next (sc, port); return s7_f (sc); }
    if (d == '\\') { tiny_next (sc, port); return tiny_read_char (sc, port); }
    if (d == 'x' || d == 'X') {
      // #x hexadecimal integer (bootstrap: only hex radix is needed)
      tiny_next (sc, port);
      bool neg = false;
      s7_int s = tiny_peek (sc, port);
      if (s == '+' || s == '-') {
        neg = (s == '-');
        tiny_next (sc, port);
      }
      long long v = 0;
      int nd = 0;
      while (true) {
        s7_int hh = tiny_peek (sc, port);
        int dd;
        if (hh >= '0' && hh <= '9') dd = hh - '0';
        else if (hh >= 'a' && hh <= 'f') dd = hh - 'a' + 10;
        else if (hh >= 'A' && hh <= 'F') dd = hh - 'A' + 10;
        else break;
        tiny_next (sc, port);
        v = v * 16 + dd;
        nd++;
      }
      if (nd == 0) {
        return s7_error (sc, s7_make_symbol (sc, "read-error"),
                         s7_list (sc, 1, s7_make_string (sc, "bad hex number")));
      }
      if (neg)
        v = -v;
      return s7_make_integer (sc, (s7_int) v);
    }
    if (d == '_') {
      // S7 #_tokens (e.g. #_list-values): read as the plain symbol "#_<name>"
      tiny_next (sc, port);
      std::string tok = "#_";
      while (!tiny_is_delim (tiny_peek (sc, port)))
        tok += (char) tiny_next (sc, port);
      return s7_make_symbol (sc, tok.c_str ());
    }
    if (d == '<') {
      // internal S7 objects: #<unspecified>, #<undefined>, #<eof>
      tiny_next (sc, port);
      std::string tok;
      while (true) {
        s7_int h = tiny_peek (sc, port);
        if (h < 0 || h == '>')
          break;
        tok += (char) tiny_next (sc, port);
      }
      if (tiny_peek (sc, port) == '>')
        tiny_next (sc, port);
      if (tok == "unspecified")
        return s7_unspecified (sc);
      if (tok == "undefined")
        return s7_undefined (sc);
      if (tok == "eof")
        return s7_eof_object (sc);
      return s7_error (sc, s7_make_symbol (sc, "read-error"),
                       s7_list (sc, 1, s7_make_string (sc, "unknown #< object")));
    }
    return s7_error (sc, s7_make_symbol (sc, "read-error"),
                     s7_list (sc, 1, s7_make_string (sc, "unknown # object")));
  }
  // consume the first char, then read the token
  tiny_next (sc, port);
  return tiny_read_token (sc, port, c);
}

static s7_pointer
f_tiny_read (s7_scheme* sc, s7_pointer args) {
  s7_pointer port = s7_car (args);
  return tiny_read_form (sc, port);
}

static s7_pointer
f_tiny_read_with_default (s7_scheme* sc, s7_pointer args) {
  if (s7_is_null (sc, args)) {
    s7_pointer ip = s7_current_input_port (sc);
    return tiny_read_form (sc, ip);
  }
  return tiny_read_form (sc, s7_car (args));
}

static s7_pointer
tiny_load_path (s7_scheme* sc, const char* path) {
  s7_pointer port = s7_open_input_file (sc, path, "r");
  s7_pointer env = s7_rootlet (sc);
  s7_pointer result = s7_unspecified (sc);
  while (true) {
    s7_pointer d = tiny_read_form (sc, port);
    if (d == s7_eof_object (sc))
      break;
    result = s7_eval (sc, d, env);
  }
  s7_close_input_port (sc, port);
  return result;
}

static s7_pointer
f_tiny_load (s7_scheme* sc, s7_pointer args) {
  const char* path = s7_string (s7_car (args));
  return tiny_load_path (sc, path);
}

static s7_pointer
f_undefined (s7_scheme* sc, s7_pointer args) {
  if (s7_is_null (sc, args))
    return s7_undefined (sc);
  const char* name = s7_string (s7_car (args));
  return s7_make_undefined (sc, name);
}

void
bootstrap_scheme_reader (s7_scheme* sc, const char* gf_lib) {
  // s7 阶段（bootstrap）只加载两个纯 s7 文件：
  //   - boot.scm：seed，提供 host 宏（define-library/import/let*-values）、
  //     loader（load-find-module-file/load-source-file）与模块 substrate
  //   - reader.scm：R7RS reader，定义 read/load/expand-eval。它自包含
  //     （不 import 任何库，只用 s7 原生可用的形式以及 boot.scm / glue
  //     提供的基础函数），因此 s7 原生可以完整加载。
  //
  // string-cursor.scm 刻意不在 s7 阶段加载：它依赖 expander 宏
  // （define-record-type）以及 (liii base)/(liii unicode) 等含 define-syntax
  // 的库，s7 原生无法处理。这些库在 expander 加载后按需加载
  // （customize_goldfish_by_mode 的 B4 阶段及 load-library!），
  // s7 阶段保持纯净。
  tiny_load_path (sc, (std::string (gf_lib) + "/liii/boot.scm").c_str ());
  tiny_load_path (sc, (std::string (gf_lib) + "/liii/reader.scm").c_str ());
}

void
glue_liii_reader (s7_scheme* sc) {
  s7_define_function (sc, "g-tiny-read", f_tiny_read, 1, 0, false,
                      "(g-tiny-read port) => datum");
  s7_define_function (sc, "g-read-token", f_g_read_token, 2, 0, false,
                      "(g-read-token port first-char) => string; reads one token up to the Scheme reader's delimiter set");
  s7_define_function (sc, "g-read-string", f_g_read_string, 1, 1, false,
                      "(g-read-string port [rdelim]) => string; reads a quoted string (opening rdelim already consumed)");
  s7_define_function (sc, "g-delimiter?", f_g_delimiter_p, 1, 0, false,
                      "(g-delimiter? ch) => boolean; R7RS delimiter predicate (single source for the reader's token boundary)");
  s7_define_function (sc, "g-tiny-load", f_tiny_load, 1, 0, false,
                      "(g-tiny-load file) => last value; loads FILE through the tiny bootstrap read");
  s7_define_function (sc, "g-undefined", f_undefined, 0, 1, false,
                      "(g-undefined [name]) => the #<undefined> object, or a named undefined #<name>");
  // replace S7's read with the tiny bootstrap read
  s7_define_function (sc, "read", f_tiny_read_with_default, 0, 1, false,
                      "(read [port]) => datum");
  // make-hook/call-with-values/etc. need `read` bound, so initialize them here
  s7_initialize_misc (sc);
}

} // namespace goldfish
