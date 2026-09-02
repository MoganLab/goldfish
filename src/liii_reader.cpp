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

#include "gf.h"
#include <string>
#include <cstdlib>
#include <fstream>

namespace goldfish {

// A tiny, non-R7RS reader used only to bootstrap the Scheme reader
// (goldfish/liii/reader.scm). It understands a small subset:
//   lists (with dotted pairs), quote abbreviation, strings, integers,
//   symbols, #t/#f, #\ characters, ; line comments.
// It deliberately does NOT handle vectors, #u8, |...| symbols, datum
// labels, #; datum comments, #! directives, etc.

static bool tiny_is_ws (gf::int_ c) {
  return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f';
}

static gf::int_ tiny_peek (gf::scheme* sc, gf::pointer port) {
  gf::pointer p = gf::peek_char (sc, port);
  return gf::is_character (p) ? (gf::int_) gf::character (p) : -1;
}

static gf::int_ tiny_next (gf::scheme* sc, gf::pointer port) {
  gf::pointer p = gf::read_char (sc, port);
  return gf::is_character (p) ? (gf::int_) gf::character (p) : -1;
}

static bool tiny_is_delim (gf::int_ c) {
  return c < 0 || tiny_is_ws (c) || c == '(' || c == ')' || c == '\'' ||
         c == '"' || c == ';' || c == '[' || c == ']';
}

static void tiny_skip_ws (gf::scheme* sc, gf::pointer port) {
  while (true) {
    gf::int_ c = tiny_peek (sc, port);
    if (c >= 0 && tiny_is_ws (c)) {
      tiny_next (sc, port);
    } else if (c == ';') {
      while (true) {
        gf::int_ d = tiny_next (sc, port);
        if (d < 0 || d == '\n')
          break;
      }
    } else {
      return;
    }
  }
}

static gf::pointer tiny_read_form (gf::scheme* sc, gf::pointer port);

static gf::pointer tiny_read_string_core (gf::scheme* sc, gf::pointer port, gf::int_ rdelim);

static gf::pointer
tiny_read_string (gf::scheme* sc, gf::pointer port) {
  tiny_next (sc, port);  // consume "
  return tiny_read_string_core (sc, port, '"');
}

static gf::pointer
tiny_read_char (gf::scheme* sc, gf::pointer port) {
  gf::int_ c = tiny_next (sc, port);
  if (c < 0) {
    return gf::error (sc, gf::make_symbol (sc, "read-error"),
                     gf::list (sc, gf::make_string (sc, "unexpected end of input in character")));
  }
  // hex escape: #\x followed by hex digits (or #\x alone = the char x)
  if (c == 'x' || c == 'X') {
    gf::int_ h = tiny_peek (sc, port);
    int d;
    if (h >= '0' && h <= '9') d = h - '0';
    else if (h >= 'a' && h <= 'f') d = h - 'a' + 10;
    else if (h >= 'A' && h <= 'F') d = h - 'A' + 10;
    else return gf::make_character (sc, c);  // plain #\x
    int v = 0;
    while (true) {
      gf::int_ hh = tiny_peek (sc, port);
      int dd;
      if (hh >= '0' && hh <= '9') dd = hh - '0';
      else if (hh >= 'a' && hh <= 'f') dd = hh - 'a' + 10;
      else if (hh >= 'A' && hh <= 'F') dd = hh - 'A' + 10;
      else break;
      tiny_next (sc, port);
      v = v * 16 + dd;
    }
    return gf::make_character (sc, v);
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
        return gf::make_character (sc, values[i]);
    }
    if (tok.size () == 1)
      return gf::make_character (sc, (int) tok[0]);
    return gf::error (sc, gf::make_symbol (sc, "read-error"),
                     gf::list (sc, gf::make_string (sc, "invalid character")));
  }
  return gf::make_character (sc, c);
}

static gf::pointer
tiny_read_token (gf::scheme* sc, gf::pointer port, gf::int_ first) {
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
    return gf::make_integer (sc, (gf::int_) v);
  }
  return gf::make_symbol (sc, tok.c_str ());
}

// The Scheme reader's delimiter set (goldfish/liii/reader.scm `delimiter?'
// delegates to g-delimiter?): whitespace, ( ) [ ] " ; .  Note that ' and `
// are NOT delimiters there (unlike tiny_is_delim, which is only used for the
// bootstrap reader).  Single source of truth: g-read-token uses the same set.
static bool scheme_delim (gf::int_ c) {
  return c < 0 || c == '(' || c == ')' || c == '[' || c == ']' ||
         c == ';' || c == '"' || c == ' ' || c == '\t' || c == '\n' ||
         c == '\r' || c == '\f' || c == '\xc';
}

static gf::pointer
f_g_delimiter_p (gf::scheme* sc, gf::pointer args) {
  gf::pointer ch = gf::car (args);
  if (!gf::is_character (ch))
    return gf::f (sc);
  return scheme_delim ((gf::int_) gf::character (ch)) ? gf::t (sc) : gf::f (sc);
}

// g-read-token : port first-char -> string
// Read one token (first-char already consumed) up to the Scheme reader's
// delimiter set, returning the raw token text.  The Scheme reader does the
// interpretation (number vs symbol, case folding), so C++ stays a thin,
// fast character pump for the hottest parsing path.
static gf::pointer
f_g_read_token (gf::scheme* sc, gf::pointer args) {
  gf::pointer port = gf::car (args);
  gf::pointer first_p = gf::cadr (args);
  gf::int_     first = gf::is_character (first_p) ? (gf::int_) gf::character (first_p)
                                                : (gf::int_) first_p;
  std::string tok;
  tok += (char) first;
  while (true) {
    gf::int_ c = tiny_peek (sc, port);
    if (scheme_delim (c))
      break;
    tok += (char) tiny_next (sc, port);
  }
  return gf::make_string (sc, tok.c_str ());
}

// R7RS 7.1.1 <identifier> char classes, mirroring the Scheme reader's
// identifier-initial?/identifier-subsequent? (chars >= 128 are valid, S7
// extension; the tables only cover 0-127).

static bool id_initial (gf::int_ c) {
  if (c < 0) return false;
  if (c >= 128) return true;
  static const bool tbl[128] = {false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,true,false,false,true,true,true,false,false,false,true,false,false,false,false,true,false,false,false,false,false,false,false,false,false,false,true,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,false,false,true,true,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,false,false,true,false};
  return tbl[c];
}

static bool id_subsequent (gf::int_ c) {
  if (c < 0) return false;
  if (c >= 128) return true;
  static const bool tbl[128] = {false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,true,false,false,true,true,true,false,false,false,true,true,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,false,false,true,true,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,false,false,true,false};
  return tbl[c];
}

static bool id_sign_subseq (gf::int_ c) {
  return c == '+' || c == '-' || c == '@' || id_initial (c);
}

static bool id_dot_subseq (gf::int_ c) {
  return c == '.' || id_sign_subseq (c);
}

// g-valid-identifier? : string -> boolean
// R7RS <identifier> check for tokens NOT beginning with a vertical bar
// (the Scheme reader routes |...| literals separately).  Mirrors the
// Scheme valid-identifier? exactly; native because it runs once per
// token read at startup (hundreds of thousands of calls).
static gf::pointer
f_g_valid_identifier_p (gf::scheme* sc, gf::pointer args) {
  gf::pointer sp = gf::car (args);
  if (!gf::is_string (sp))
    return gf::f (sc);
  std::string s = gf::string (sp);
  size_t len = s.size ();
  if (len == 0)
    return gf::f (sc);
  gf::int_ c0 = (unsigned char) s[0];
  bool ok;
  if (id_initial (c0)) {
    ok = true;
    for (size_t i = 1; i < len; i++)
      if (!id_subsequent ((unsigned char) s[i])) { ok = false; break; }
  } else if (c0 == '+' || c0 == '-') {
    if (len == 1) {
      ok = true;
    } else {
      gf::int_ c1 = (unsigned char) s[1];
      if (c1 == '.') {
        ok = len > 2 && id_dot_subseq ((unsigned char) s[2]);
        for (size_t i = 3; ok && i < len; i++)
          if (!id_subsequent ((unsigned char) s[i])) { ok = false; break; }
      } else if (id_sign_subseq (c1)) {
        ok = true;
        for (size_t i = 2; i < len; i++)
          if (!id_subsequent ((unsigned char) s[i])) { ok = false; break; }
      } else {
        ok = false;
      }
    }
  } else if (c0 == '.') {
    ok = len > 1 && id_dot_subseq ((unsigned char) s[1]);
    for (size_t i = 2; ok && i < len; i++)
      if (!id_subsequent ((unsigned char) s[i])) { ok = false; break; }
  } else {
    ok = false;
  }
  return ok ? gf::t (sc) : gf::f (sc);
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

static gf::pointer
tiny_read_string_core (gf::scheme* sc, gf::pointer port, gf::int_ rdelim) {
  std::string s;
  while (true) {
    gf::int_ c = tiny_next (sc, port);
    if (c < 0) {
      return gf::error (sc, gf::make_symbol (sc, "read-error"),
                       gf::list (sc, gf::make_string (sc, "unterminated string")));
    }
    if (c == rdelim)
      break;
    if (c != '\\') {
      s += (char) c;
      continue;
    }
    gf::int_ e = tiny_next (sc, port);
    if (e < 0) {
      return gf::error (sc, gf::make_symbol (sc, "read-error"),
                       gf::list (sc, gf::make_string (sc, "unterminated string")));
    }
    if (e == '\n' || e == '\r') {
      // line continuation: consume the whole line ending, then intraline ws
      if (e == '\r' && tiny_peek (sc, port) == '\n')
        tiny_next (sc, port);
      while (true) {
        gf::int_ p = tiny_peek (sc, port);
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
          gf::int_ h = tiny_peek (sc, port);
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
          return gf::error (sc, gf::make_symbol (sc, "read-error"),
                           gf::list (sc, gf::make_string (sc, "invalid hex escape")));
        }
        if (tiny_peek (sc, port) != ';') {
          return gf::error (sc, gf::make_symbol (sc, "read-error"),
                           gf::list (sc, gf::make_string (sc, "hex escape missing semicolon")));
        }
        tiny_next (sc, port);
        g_append_utf8 (s, v);
        break;
      }
      default:
        return gf::error (sc, gf::make_symbol (sc, "read-error"),
                         gf::list (sc, gf::make_string (sc, "invalid character in escape sequence")));
    }
  }
  return gf::make_string_with_length (sc, s.c_str (), (gf::int_) s.size ());
}

static gf::pointer
f_g_read_string (gf::scheme* sc, gf::pointer args) {
  gf::pointer port = gf::car (args);
  gf::int_     rdelim = '"';
  if (gf::is_character (gf::cadr (args)))
    rdelim = (gf::int_) gf::character (gf::cadr (args));
  return tiny_read_string_core (sc, port, rdelim);
}

static gf::pointer
tiny_read_form (gf::scheme* sc, gf::pointer port) {
  tiny_skip_ws (sc, port);
  gf::int_ c = tiny_peek (sc, port);
  if (c < 0)
    return gf::eof_object (sc);
  if (c == '(' || c == '[') {
    const int close = (c == '(') ? ')' : ']';
    tiny_next (sc, port);
    gf::pointer head = gf::nil (sc);
    gf::pointer tail = gf::nil (sc);
    bool first = true;
    while (true) {
      tiny_skip_ws (sc, port);
      gf::int_ d = tiny_peek (sc, port);
      if (d < 0) {
        return gf::error (sc, gf::make_symbol (sc, "read-error"),
                         gf::list (sc, gf::make_string (sc, "unterminated list")));
      }
      if (d == close) {
        tiny_next (sc, port);
        return head;
      }
      if (d == '.' && (first || true)) {
        // dotted pair: require a delimiter after the dot
        gf::int_ after = tiny_peek (sc, port);
        (void) after;
        // peek past '.' without consuming: the dot is standalone iff the
        // following char is a delimiter
        tiny_next (sc, port);
        if (tiny_is_delim (tiny_peek (sc, port))) {
          if (first) {
            return gf::error (sc, gf::make_symbol (sc, "read-error"),
                             gf::list (sc, gf::make_string (sc, "dot with no element")));
          }
          gf::pointer b = tiny_read_form (sc, port);
          tiny_skip_ws (sc, port);
          if (tiny_peek (sc, port) != close) {
            return gf::error (sc, gf::make_symbol (sc, "read-error"),
                             gf::list (sc, gf::make_string (sc, "bad dotted pair")));
          }
          tiny_next (sc, port);
          gf::set_cdr (tail, b);
          return head;
        }
        // not a standalone dot: fall through and read it as a token
        return tiny_read_token (sc, port, '.');
      }
      gf::pointer el = tiny_read_form (sc, port);
      if (first) {
        head = gf::cons (sc, el, gf::nil (sc));
        tail = head;
        first = false;
      } else {
        gf::pointer cell = gf::cons (sc, el, gf::nil (sc));
        gf::set_cdr (tail, cell);
        tail = cell;
      }
    }
  }
  if (c == ')' || c == ']') {
    tiny_next (sc, port);
    return gf::error (sc, gf::make_symbol (sc, "read-error"),
                     gf::list (sc, gf::make_string (sc, "unexpected )")));
  }
  if (c == '\'') {
    tiny_next (sc, port);
    return gf::cons (sc, gf::make_symbol (sc, "quote"),
                    gf::cons (sc, tiny_read_form (sc, port), gf::nil (sc)));
  }
  if (c == '`') {
    tiny_next (sc, port);
    return gf::cons (sc, gf::make_symbol (sc, "quasiquote"),
                    gf::cons (sc, tiny_read_form (sc, port), gf::nil (sc)));
  }
  if (c == ',') {
    tiny_next (sc, port);
    const char* name = "unquote";
    if (tiny_peek (sc, port) == '@') {
      tiny_next (sc, port);
      name = "unquote-splicing";
    }
    return gf::cons (sc, gf::make_symbol (sc, name),
                    gf::cons (sc, tiny_read_form (sc, port), gf::nil (sc)));
  }
  if (c == '"')
    return tiny_read_string (sc, port);
  if (c == '#') {
    tiny_next (sc, port);
    gf::int_ d = tiny_peek (sc, port);
    if (d == 't') { tiny_next (sc, port); return gf::t (sc); }
    if (d == 'f') { tiny_next (sc, port); return gf::f (sc); }
    if (d == '\\') { tiny_next (sc, port); return tiny_read_char (sc, port); }
    if (d == 'x' || d == 'X') {
      // #x hexadecimal integer (bootstrap: only hex radix is needed)
      tiny_next (sc, port);
      bool neg = false;
      gf::int_ s = tiny_peek (sc, port);
      if (s == '+' || s == '-') {
        neg = (s == '-');
        tiny_next (sc, port);
      }
      long long v = 0;
      int nd = 0;
      while (true) {
        gf::int_ hh = tiny_peek (sc, port);
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
        return gf::error (sc, gf::make_symbol (sc, "read-error"),
                         gf::list (sc, gf::make_string (sc, "bad hex number")));
      }
      if (neg)
        v = -v;
      return gf::make_integer (sc, (gf::int_) v);
    }
    if (d == '_') {
      // S7 #_tokens (e.g. #_list-values): read as the plain symbol "#_<name>"
      tiny_next (sc, port);
      std::string tok = "#_";
      while (!tiny_is_delim (tiny_peek (sc, port)))
        tok += (char) tiny_next (sc, port);
      return gf::make_symbol (sc, tok.c_str ());
    }
    if (d == '<') {
      // internal S7 objects: #<unspecified>, #<undefined>, #<eof>
      tiny_next (sc, port);
      std::string tok;
      while (true) {
        gf::int_ h = tiny_peek (sc, port);
        if (h < 0 || h == '>')
          break;
        tok += (char) tiny_next (sc, port);
      }
      if (tiny_peek (sc, port) == '>')
        tiny_next (sc, port);
      if (tok == "unspecified")
        return gf::unspecified (sc);
      if (tok == "undefined")
        return gf::undefined (sc);
      if (tok == "eof")
        return gf::eof_object (sc);
      return gf::error (sc, gf::make_symbol (sc, "read-error"),
                       gf::list (sc, gf::make_string (sc, "unknown #< object")));
    }
    return gf::error (sc, gf::make_symbol (sc, "read-error"),
                     gf::list (sc, gf::make_string (sc, "unknown # object")));
  }
  // consume the first char, then read the token
  tiny_next (sc, port);
  return tiny_read_token (sc, port, c);
}

static gf::pointer
f_tiny_read (gf::scheme* sc, gf::pointer args) {
  gf::pointer port = gf::car (args);
  return tiny_read_form (sc, port);
}

static gf::pointer
f_tiny_read_with_default (gf::scheme* sc, gf::pointer args) {
  if (gf::is_null (sc, args)) {
    gf::pointer ip = gf::current_input_port (sc);
    return tiny_read_form (sc, ip);
  }
  return tiny_read_form (sc, gf::car (args));
}

static gf::pointer
tiny_load_path (gf::scheme* sc, const char* path) {
  gf::pointer port = gf::open_input_file (sc, path, "r");
  gf::pointer env = gf::rootlet (sc);
  gf::pointer result = gf::unspecified (sc);
  while (true) {
    gf::pointer d = tiny_read_form (sc, port);
    if (d == gf::eof_object (sc))
      break;
    result = gf::eval (sc, d, env);
  }
  gf::close_input_port (sc, port);
  return result;
}

static gf::pointer
f_tiny_load (gf::scheme* sc, gf::pointer args) {
  const char* path = gf::string (gf::car (args));
  return tiny_load_path (sc, path);
}

static gf::pointer
f_undefined (gf::scheme* sc, gf::pointer args) {
  if (gf::is_null (sc, args))
    return gf::undefined (sc);
  const char* name = gf::string (gf::car (args));
  return gf::make_undefined (sc, name);
}

// ---------------------------------------------------------------------------
// g-read-gfo-file : path -> datum
// Parse one whole machine-written .gfo cache file into live s7 objects.
// Replaces the interpreted read-forms (goldfish/liii/reader.scm) on the
// warm-load hot path: the pure-Scheme reader dispatches every token through
// the evaluator, which is the dominant .gfo load cost.  write-roundtrip
// emits a strict data subset -- lists/dotted pairs, integer literals,
// identifiers, strings, characters, booleans -- so this reader covers
// exactly that and rejects anything else (quote sugar, vectors, datum
// labels, #g records) with a read-error rather than guessing.
// ---------------------------------------------------------------------------

static bool gfo_delim (int c) {
  return c < 0 || c == '(' || c == ')' || c == '[' || c == ']' ||
         c == ';' || c == '"' || c == ' ' || c == '\t' || c == '\n' ||
         c == '\r' || c == '\f';
}

struct gfo_reader {
  gf::scheme* sc;
  const char* p;
  const char* end;

  bool at_end () const { return p >= end; }
  int peek () const { return at_end () ? -1 : (unsigned char) *p; }
  int peek2 () const { return (p + 1) >= end ? -1 : (unsigned char) p[1]; }
  int next () { return at_end () ? -1 : (unsigned char) *p++; }
  gf::pointer fail (const char* msg) {
    return gf::error (sc, gf::make_symbol (sc, "read-error"),
                      gf::list (sc, gf::make_string (sc, msg)));
  }
  void skip_ws () {
    for (;;) {
      int c = peek ();
      if (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f') {
        next ();
        continue;
      }
      if (c == ';') {
        while (peek () >= 0 && peek () != '\n')
          next ();
        continue;
      }
      break;
    }
  }
  gf::pointer read_form ();
  gf::pointer read_list (int close);
  gf::pointer read_string ();
  gf::pointer read_char ();
  gf::pointer read_atom (int first);
  gf::pointer read_vector ();
};

gf::pointer
gfo_reader::read_list (int close) {
  next ();  // consume '(' or '['
  gf::pointer head = gf::nil (sc);
  gf::pointer tail = gf::nil (sc);
  bool first = true;
  for (;;) {
    skip_ws ();
    int c = peek ();
    if (c < 0)
      return fail ("unterminated list");
    if (c == close) {
      next ();
      return head;
    }
    if (c == ')' || c == ']')
      return fail ("unexpected close paren");
    if (c == '.') {
      // standalone dot iff followed by a delimiter
      next ();
      int after = peek ();
      if (gfo_delim (after)) {
        if (first)
          return fail ("dot with no element");
        gf::pointer b = read_form ();
        skip_ws ();
        if (peek () != close)
          return fail ("bad dotted pair");
        next ();
        gf::set_cdr (tail, b);
        return head;
      }
      gf::pointer el = read_atom ('.');
      gf::pointer cell = gf::cons (sc, el, gf::nil (sc));
      if (first) {
        head = cell;
        tail = cell;
        first = false;
      } else {
        gf::set_cdr (tail, cell);
        tail = cell;
      }
      continue;
    }
    gf::pointer el = read_form ();
    gf::pointer cell = gf::cons (sc, el, gf::nil (sc));
    if (first) {
      head = cell;
      tail = cell;
      first = false;
    } else {
      gf::set_cdr (tail, cell);
      tail = cell;
    }
  }
}

gf::pointer
gfo_reader::read_string () {
  next ();  // consume '"'
  std::string s;
  for (;;) {
    int c = next ();
    if (c < 0)
      return fail ("unterminated string");
    if (c == '"')
      break;
    if (c != '\\') {
      s += (char) c;
      continue;
    }
    int e = next ();
    if (e < 0)
      return fail ("unterminated string");
    if (e == '\n' || e == '\r') {
      if (e == '\r' && peek () == '\n')
        next ();
      while (peek () == ' ' || peek () == '\t')
        next ();
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
          int h = peek ();
          int d;
          if (h >= '0' && h <= '9') d = h - '0';
          else if (h >= 'a' && h <= 'f') d = h - 'a' + 10;
          else if (h >= 'A' && h <= 'F') d = h - 'A' + 10;
          else break;
          next ();
          v = v * 16 + d;
          nd++;
        }
        if (nd == 0)
          return fail ("invalid hex escape");
        if (peek () != ';')
          return fail ("hex escape missing semicolon");
        next ();
        g_append_utf8 (s, v);
        break;
      }
      default:
        return fail ("invalid character in escape sequence");
    }
  }
  return gf::make_string_with_length (sc, s.c_str (), (gf::int_) s.size ());
}

gf::pointer
gfo_reader::read_char () {
  int c = next ();  // consume '\\'
  if (c < 0)
    return fail ("unexpected end of input in character");
  if (c == 'x' || c == 'X') {
    int h = peek ();
    int d;
    if (h >= '0' && h <= '9') d = h - '0';
    else if (h >= 'a' && h <= 'f') d = h - 'a' + 10;
    else if (h >= 'A' && h <= 'F') d = h - 'A' + 10;
    else return gf::make_character (sc, c);  // plain #\x
    int v = 0;
    while (true) {
      int hh = peek ();
      int dd;
      if (hh >= '0' && hh <= '9') dd = hh - '0';
      else if (hh >= 'a' && hh <= 'f') dd = hh - 'a' + 10;
      else if (hh >= 'A' && hh <= 'F') dd = hh - 'A' + 10;
      else break;
      next ();
      v = v * 16 + dd;
    }
    return gf::make_character (sc, v);
  }
  // multi-byte UTF-8 codepoint (the .gfo character literals include
  // non-ASCII R7RS-ish chars like #\〇); decode lead byte + continuations.
  if (c >= 0xc0) {
    int v = c & ((c < 0xe0) ? 0x1f : (c < 0xf0 ? 0x0f : 0x07));
    int extra = (c < 0xe0) ? 1 : (c < 0xf0 ? 2 : 3);
    for (int i = 0; i < extra; i++) {
      int b = next ();
      if (b < 0 || (b & 0xc0) != 0x80)
        return fail ("invalid UTF-8 in character");
      v = (v << 6) | (b & 0x3f);
    }
    return gf::make_character (sc, v);
  }
  if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
    std::string tok;
    tok += (char) c;
    while (!gfo_delim (peek ()))
      tok += (char) next ();
    static const char* names[] = {
      "alarm", "backspace", "delete", "escape", "newline", "null",
      "return", "space", "tab"
    };
    static const int values[] = {7, 8, 127, 27, 10, 0, 13, 32, 9};
    for (size_t i = 0; i < 9; i++)
      if (tok == names[i])
        return gf::make_character (sc, values[i]);
    if (tok.size () == 1)
      return gf::make_character (sc, (int) tok[0]);
    return fail ("invalid character");
  }
  return gf::make_character (sc, c);
}

gf::pointer
gfo_reader::read_vector () {
  next ();  // consume '(' after '#'
  gf::pointer head = gf::nil (sc);
  gf::pointer tail = gf::nil (sc);
  int n = 0;
  for (;;) {
    skip_ws ();
    int c = peek ();
    if (c < 0)
      return fail ("unterminated vector");
    if (c == ')') {
      next ();
      break;
    }
    if (c == '.')
      return fail ("dotted vector");
    gf::pointer el = read_form ();
    gf::pointer cell = gf::cons (sc, el, gf::nil (sc));
    if (n == 0) {
      head = cell;
      tail = cell;
    } else {
      gf::set_cdr (tail, cell);
      tail = cell;
    }
    n++;
  }
  gf::pointer v = gf::make_vector (sc, n);
  gf::pointer walk = head;
  for (int i = 0; i < n; i++) {
    gf::vector_set (sc, v, i, gf::car (walk));
    walk = gf::cdr (walk);
  }
  return v;
}

gf::pointer
gfo_reader::read_atom (int first) {
  std::string tok;
  tok += (char) first;
  while (!gfo_delim (peek ()))
    tok += (char) next ();
  // integer fast path (the common .gfo numeric literal)
  {
    const char* s = tok.c_str ();
    char* endp = nullptr;
    long long v = strtoll (s, &endp, 10);
    if (endp == s + tok.size ())
      return gf::make_integer (sc, (gf::int_) v);
  }
  // identifier: mirror the Scheme reader's symbol path (no case folding --
  // fold-case? defaults to #f and .gfo symbols are read without set-fold-case!)
  {
    bool ok;
    const size_t len = tok.size ();
    if (len == 0) ok = false;
    else {
      int c0 = (unsigned char) tok[0];
      if (id_initial (c0)) {
        ok = true;
        for (size_t i = 1; i < len; i++)
          if (!id_subsequent ((unsigned char) tok[i])) { ok = false; break; }
      } else if (c0 == '+' || c0 == '-') {
        if (len == 1) {
          ok = true;
        } else {
          int c1 = (unsigned char) tok[1];
          if (c1 == '.') {
            ok = len > 2 && id_dot_subseq ((unsigned char) tok[2]);
            for (size_t i = 3; ok && i < len; i++)
              if (!id_subsequent ((unsigned char) tok[i])) { ok = false; break; }
          } else if (id_sign_subseq (c1)) {
            ok = true;
            for (size_t i = 2; i < len; i++)
              if (!id_subsequent ((unsigned char) tok[i])) { ok = false; break; }
          } else {
            ok = false;
          }
        }
      } else if (c0 == '.') {
        ok = len > 1 && id_dot_subseq ((unsigned char) tok[1]);
        for (size_t i = 2; ok && i < len; i++)
          if (!id_subsequent ((unsigned char) tok[i])) { ok = false; break; }
      } else {
        ok = false;
      }
    }
    if (ok)
      return gf::make_symbol (sc, tok.c_str ());
  }
  // number fallback: a token that is neither an integer nor an identifier is
  // interpreted through s7's own number parser (reals, ratios, exponent
  // notation, imaginary forms -- e.g. write-roundtrip writes a float as
  // "1000000.0" or "1e6").  Only accept a number; anything else is an error.
  {
    const char* s = tok.c_str ();
    char* endp = nullptr;
    double d = strtod (s, &endp);
    if (endp == s + tok.size ())
      return gf::make_real (sc, d);
  }
  {
    gf::pointer s2n = gf::name_to_value (sc, "string->number");
    gf::pointer tok_p = gf::make_string (sc, tok.c_str ());
    gf::pointer v = gf::call (sc, s2n, gf::list (sc, tok_p));
    if (gf::is_number (v))
      return v;
  }
  std::string msg = "invalid token: [" + tok + "]";
  return fail (msg.c_str ());
}

gf::pointer
gfo_reader::read_form () {
  skip_ws ();
  int c = peek ();
  if (c < 0)
    return gf::eof_object (sc);
  if (c == '(' || c == '[')
    return read_list (c == '(' ? ')' : ']');
  if (c == ')')
    return fail ("unexpected )");
  if (c == ']')
    return fail ("unexpected ]");
  if (c == '"')
    return read_string ();
  if (c == '\\')
    return read_char ();
  if (c == '|') {
    // |...| symbol literal: write-roundtrip-symbol escapes only | and \
    next ();
    std::string s;
    for (;;) {
      int e = next ();
      if (e < 0)
        return fail ("unterminated |...| symbol");
      if (e == '|')
        break;
      if (e == '\\') {
        int e2 = next ();
        if (e2 < 0)
          return fail ("unterminated |...| symbol");
        if (e2 == '|' || e2 == '\\')
          s += (char) e2;
        else {
          s += '\\';
          s += (char) e2;
        }
        continue;
      }
      s += (char) e;
    }
    return gf::make_symbol (sc, s.c_str ());
  }
  if (c == '\'' || c == '`' || c == ',')
    return fail ("quote abbreviations are not emitted by write-roundtrip");
  if (c == '#') {
    next ();
    int d = peek ();
    if (d == 't' || d == 'T') {
      next ();
      while (peek () >= 0 && !gfo_delim (peek ()))  // consume 'true'
        next ();
      return gf::t (sc);
    }
    if (d == 'f' || d == 'F') {
      next ();
      while (peek () >= 0 && !gfo_delim (peek ()))  // consume 'false'
        next ();
      return gf::f (sc);
    }
    if (d == '\\') {
      next ();
      return read_char ();
    }
    if (d == '(') {
      return read_vector ();
    }
    if (d == 'b' || d == 'B' || d == 'o' || d == 'O' || d == 'd' || d == 'D' ||
        d == 'x' || d == 'X' || d == 'e' || d == 'E' || d == 'i' || d == 'I') {
      // #x...#e... prefixed number: read the whole #-token, interpret via s7
      std::string tok = "#";
      while (peek () >= 0 && !gfo_delim (peek ()))
        tok += (char) next ();
      {
        gf::pointer s2n = gf::name_to_value (sc, "string->number");
        gf::pointer tok_p = gf::make_string (sc, tok.c_str ());
        gf::pointer v = gf::call (sc, s2n, gf::list (sc, tok_p));
        if (gf::is_number (v))
          return v;
      }
      return fail ("invalid prefixed number");
    }
    if (d == 'u') {
      // #u8(...) bytevector: reject (write-roundtrip emits none for current payloads)
      next ();
      if (peek () == '8' && peek2 () == '(')
        return fail ("bytevectors are not emitted by write-roundtrip");
      return fail ("unsupported # object in .gfo");
    }
    return fail ("unsupported # object in .gfo");
  }
  // consume the first char, then read the rest of the token
  next ();
  return read_atom (c);
}

static gf::pointer
f_g_read_gfo_file (gf::scheme* sc, gf::pointer args) {
  const char* path = gf::string (gf::car (args));
  std::ifstream in (path, std::ios::in | std::ios::binary);
  if (!in)
    return gf::error (sc, gf::make_symbol (sc, "read-error"),
                      gf::list (sc, gf::make_string (sc, "g-read-gfo-file: cannot open file"),
                                gf::make_string (sc, path)));
  std::string buf ((std::istreambuf_iterator<char> (in)), std::istreambuf_iterator<char> ());
  gfo_reader r {sc, buf.data (), buf.data () + buf.size ()};
  gf::pointer form = r.read_form ();
  r.skip_ws ();
  if (!r.at_end ())
    return gf::error (sc, gf::make_symbol (sc, "read-error"),
                      gf::list (sc, gf::make_string (sc, "g-read-gfo-file: trailing data")));
  return form;
}

void
bootstrap_scheme_reader (gf::scheme* sc, const char* gf_lib) {
  // s7 phase: load boot.scm (seed loader) + host-abi.scm (R7RS value surface).
  // reader.scm and string-cursor.scm are not loaded here; they need the expander
  // (syntax-rules/define-record-type) and are loaded after the artifact.
  tiny_load_path (sc, (std::string (gf_lib) + "/liii/boot.scm").c_str ());
  tiny_load_path (sc, (std::string (gf_lib) + "/liii/host-abi.scm").c_str ());
}

void
glue_liii_reader (gf::scheme* sc) {
  gf::define_function (sc, "g-tiny-read", f_tiny_read, 1, 0, false,
                      "(g-tiny-read port) => datum");
  gf::define_function (sc, "g-read-token", f_g_read_token, 2, 0, false,
                      "(g-read-token port first-char) => string; reads one token up to the Scheme reader's delimiter set");
  gf::define_function (sc, "g-valid-identifier?", f_g_valid_identifier_p, 1, 0, false,
                      "(g-valid-identifier? string) => boolean; R7RS <identifier> check (native: hot path at startup)");
  gf::define_function (sc, "g-read-string", f_g_read_string, 1, 1, false,
                      "(g-read-string port [rdelim]) => string; reads a quoted string (opening rdelim already consumed)");
  gf::define_function (sc, "g-delimiter?", f_g_delimiter_p, 1, 0, false,
                      "(g-delimiter? ch) => boolean; R7RS delimiter predicate (single source for the reader's token boundary)");
  gf::define_function (sc, "g-tiny-load", f_tiny_load, 1, 0, false,
                      "(g-tiny-load file) => last value; loads FILE through the tiny bootstrap read");
  gf::define_function (sc, "g-read-gfo-file", f_g_read_gfo_file, 1, 0, false,
                      "(g-read-gfo-file path) => datum; natively parses one whole machine-written .gfo cache file (write-roundtrip data subset)");
  gf::define_function (sc, "g-undefined", f_undefined, 0, 1, false,
                      "(g-undefined [name]) => the #<undefined> object, or a named undefined #<name>");
  // replace S7's read with the tiny bootstrap read
  gf::define_function (sc, "read", f_tiny_read_with_default, 0, 1, false,
                      "(read [port]) => datum");
  // make-hook/call-with-values/etc. need `read` bound, so initialize them here
  gf::initialize_misc (sc);
}

} // namespace goldfish
