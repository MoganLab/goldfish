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
         c == '"' || c == ';';
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

static s7_pointer
tiny_read_string (s7_scheme* sc, s7_pointer port) {
  tiny_next (sc, port);  // consume "
  std::string s;
  while (true) {
    s7_int c = tiny_next (sc, port);
    if (c < 0) {
      return s7_error (sc, s7_make_symbol (sc, "read-error"),
                       s7_list (sc, 1, s7_make_string (sc, "unterminated string")));
    }
    if (c == '"')
      break;
    if (c == '\\') {
      s7_int e = tiny_next (sc, port);
      if (e < 0) {
        return s7_error (sc, s7_make_symbol (sc, "read-error"),
                         s7_list (sc, 1, s7_make_string (sc, "unterminated string")));
      }
      switch (e) {
        case 'n':  s += '\n'; break;
        case 't':  s += '\t'; break;
        case 'r':  s += '\r'; break;
        case 'a':  s += '\a'; break;
        case 'b':  s += '\b'; break;
        case 'v':  s += '\v'; break;
        case 'f':  s += '\f'; break;
        case '\\': s += '\\'; break;
        case '"':  s += '"'; break;
        case 'x': {
          int v = 0;
          while (true) {
            s7_int h = tiny_peek (sc, port);
            int d;
            if (h >= '0' && h <= '9') d = h - '0';
            else if (h >= 'a' && h <= 'f') d = h - 'a' + 10;
            else if (h >= 'A' && h <= 'F') d = h - 'A' + 10;
            else break;
            tiny_next (sc, port);
            v = v * 16 + d;
          }
          if (tiny_peek (sc, port) != ';') {
            return s7_error (sc, s7_make_symbol (sc, "read-error"),
                             s7_list (sc, 1, s7_make_string (sc, "hex escape missing semicolon")));
          }
          tiny_next (sc, port);
          s += (char) v;  // bootstrap strings are byte strings
          break;
        }
        default:
          return s7_error (sc, s7_make_symbol (sc, "read-error"),
                           s7_list (sc, 1, s7_make_string (sc, "invalid string escape")));
      }
    } else {
      s += (char) c;
    }
  }
  return s7_make_string (sc, s.c_str ());
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

static s7_pointer
tiny_read_form (s7_scheme* sc, s7_pointer port) {
  tiny_skip_ws (sc, port);
  s7_int c = tiny_peek (sc, port);
  if (c < 0)
    return s7_eof_object (sc);
  if (c == '(') {
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
      if (d == ')') {
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
          if (tiny_peek (sc, port) != ')') {
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
  if (c == ')') {
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
f_s7_read (s7_scheme* sc, s7_pointer args) {
  // S7's original C parser, kept for benchmarking the bootstrap read
  return s7_read (sc, s7_car (args));
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

void
bootstrap_scheme_reader (s7_scheme* sc, const char* gf_lib) {
  // the tiny bootstrap read loads boot.scm, string-cursor.scm and reader.scm;
  // reader.scm ends by defining `read` and `load` (through the Scheme reader)
  tiny_load_path (sc, (std::string (gf_lib) + "/scheme/boot.scm").c_str ());
  tiny_load_path (sc, (std::string (gf_lib) + "/liii/string-cursor.scm").c_str ());
  tiny_load_path (sc, (std::string (gf_lib) + "/liii/reader.scm").c_str ());
}

void
glue_liii_reader (s7_scheme* sc) {
  s7_define_function (sc, "g-tiny-read", f_tiny_read, 1, 0, false,
                      "(g-tiny-read port) => datum");
  s7_define_function (sc, "g-s7-read", f_s7_read, 1, 0, false,
                      "(g-s7-read port) => datum; S7's original C reader, for benchmarking");
  s7_define_function (sc, "g-tiny-load", f_tiny_load, 1, 0, false,
                      "(g-tiny-load file) => last value; loads FILE through the tiny bootstrap read");
  // replace S7's read with the tiny bootstrap read
  s7_define_function (sc, "read", f_tiny_read_with_default, 0, 1, false,
                      "(read [port]) => datum");
}

} // namespace goldfish
