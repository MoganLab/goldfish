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
#include <limits>
#include <vector>

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

static gf::pointer tiny_read_form (gf::scheme* sc, gf::pointer port, struct TinyLabels& lab);

static gf::pointer tiny_read_string_core (gf::scheme* sc, gf::pointer port, gf::int_ rdelim);

// Graph labels (#n= def / #n# ref) the write-roundtrip graph pass emits
// for shared structure: without them a cache round trip silently
// duplicates shared vectors (mutation through one alias lost through
// the other). Objects are GC-protected from definition until the
// entry-point read returns; the owner unprotects them all.
struct TinyLabels {
  std::vector<gf::pointer> objs;
  std::vector<int> locs;
};

static void
tiny_labels_store (gf::scheme* sc, TinyLabels& lab, size_t n, gf::pointer obj) {
  if (n >= lab.objs.size ()) {
    lab.objs.resize (n + 1, nullptr);
    lab.locs.resize (n + 1, -1);
  }
  if (lab.locs[n] >= 0) gf::gc_unprotect_at (sc, lab.locs[n]);
  lab.objs[n] = obj;
  lab.locs[n] = gf::gc_protect (sc, obj);
}

static void
tiny_labels_release (gf::scheme* sc, TinyLabels& lab) {
  for (int loc : lab.locs)
    if (loc >= 0) gf::gc_unprotect_at (sc, loc);
  lab.locs.clear ();
  lab.objs.clear ();
}

static gf::pointer tiny_read_string_core (gf::scheme* sc, gf::pointer port, gf::int_ rdelim);

// Read a #(...) vector: elements are read like a list (no dotted pair) into
// a GC-protected list, then materialized as an s7 vector.
static gf::pointer
tiny_read_vector (gf::scheme* sc, gf::pointer port, TinyLabels& lab) {
  tiny_next (sc, port);  // consume '('
  gf::pointer head = gf::nil (sc);
  gf::pointer tail = gf::nil (sc);
  int head_loc = -1;
  while (true) {
    tiny_skip_ws (sc, port);
    gf::int_ d = tiny_peek (sc, port);
    if (d < 0)
      return gf::error (sc, gf::make_symbol (sc, "read-error"),
                       gf::list (sc, gf::make_string (sc, "unterminated vector")));
    if (d == ')') {
      tiny_next (sc, port);
      break;
    }
    gf::pointer el = tiny_read_form (sc, port, lab);
    if (head_loc < 0) {
      head = gf::cons (sc, el, gf::nil (sc));
      tail = head;
      head_loc = gf::gc_protect (sc, head);
    } else {
      gf::pointer cell = gf::cons (sc, el, gf::nil (sc));
      gf::set_cdr (tail, cell);
      tail = cell;
    }
  }
  // count elements, then fill the vector
  int n = 0;
  for (gf::pointer p = head; !gf::is_null (sc, p); p = gf::cdr (p))
    n++;
  gf::pointer vec = gf::make_vector (sc, n);
  int i = 0;
  for (gf::pointer p = head; !gf::is_null (sc, p); p = gf::cdr (p)) {
    gf::vector_set (sc, vec, i, gf::car (p));
    i++;
  }
  if (head_loc >= 0)
    gf::gc_unprotect_at (sc, head_loc);
  return vec;
}


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
  // A character literal may be a multi-byte UTF-8 code point (written bare
  // as e.g. #\　): decode the continuation bytes when the first byte is a
  // UTF-8 lead byte, so the literal reads back as the full code point.
  if (c >= 0xc2 && c <= 0xf4) {
    int extra = (c <= 0xdf) ? 1 : (c <= 0xef) ? 2 : 3;
    int v = c & ((c <= 0xdf) ? 0x1f : (c <= 0xef) ? 0x0f : 0x07);
    for (int i = 0; i < extra; i++) {
      gf::int_ b = tiny_next (sc, port);
      if (b < 0x80 || b > 0xbf)
        return gf::error (sc, gf::make_symbol (sc, "read-error"),
                         gf::list (sc, gf::make_string (sc, "invalid UTF-8 in character")));
      v = (v << 6) | (b & 0x3f);
    }
    return gf::make_character (sc, v);
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

// Parse [lo,hi) of tok as an inexact real part: [sign] (digits[.digits][exp]
// | digits/digits | inf.0 | nan.0). Whole-span or nothing; never errors.
// s7 reads every a+bi inexact, so doubles match the full reader exactly.
static bool
tiny_parse_part (const std::string& tok, size_t lo, size_t hi, double& out) {
  size_t p = lo;
  double sign = 1.0;
  if (p < hi && (tok[p] == '+' || tok[p] == '-')) {
    if (tok[p] == '-') sign = -1.0;
    p++;
  }
  if (p >= hi) return false;
  auto rest_is = [&] (const char* w) -> bool {
    size_t k = p;
    for (size_t j = 0; w[j]; j++, k++)
      if (k >= hi || tok[k] != w[j]) return false;
    return k == hi;
  };
  if (rest_is ("inf.0")) {
    out = sign * std::numeric_limits<double>::infinity ();
    return true;
  }
  if (rest_is ("nan.0")) {
    out = sign * std::numeric_limits<double>::quiet_NaN ();
    return true;
  }
  size_t ds = p;
  while (p < hi && tok[p] >= '0' && tok[p] <= '9') p++;
  if (p < hi && tok[p] == '/' && p > ds) {
    size_t ne = p++;
    size_t qs = p;
    while (p < hi && tok[p] >= '0' && tok[p] <= '9') p++;
    if (p != hi || p == qs) return false;
    double num = std::strtod (tok.substr (ds, ne - ds).c_str (), nullptr);
    double den = std::strtod (tok.substr (qs, p - qs).c_str (), nullptr);
    if (den == 0.0) return false;
    out = sign * num / den;
    return true;
  }
  if (p == ds && (p >= hi || tok[p] != '.')) return false;
  std::string sub = tok.substr (lo, hi - lo);
  for (char c : sub)
    if (c == 'x' || c == 'X' || c == 'p' || c == 'P') return false;
  char* end = nullptr;
  double d = std::strtod (sub.c_str (), &end);
  if (end == sub.c_str () + static_cast<std::ptrdiff_t> (sub.size ())) {
    out = d;
    return true;
  }
  return false;
}

// Parse tok as rectangular complex (lowercase i only: 3+4I errors in s7,
// bare i is a symbol). Split at the last +/- that is not an exponent
// marker (1e3+2e-2i); empty imag after a sign means 1 (1+i); no split
// means pure imaginary (+i, 2i). Parts reuse tiny_parse_part.
static bool
tiny_parse_complex (const std::string& tok, double& re, double& im) {
  if (tok.empty () || tok.back () != 'i') return false;
  std::string body = tok.substr (0, tok.size () - 1);
  if (body.empty ()) return false;
  size_t signpos = (body[0] == '+' || body[0] == '-') ? 1 : 0;
  size_t split = std::string::npos;
  for (size_t k = body.size (); k-- > signpos;) {
    if ((body[k] == '+' || body[k] == '-') &&
        !(k > 0 && (body[k - 1] == 'e' || body[k - 1] == 'E'))) {
      split = k;
      break;
    }
  }
  if (split == std::string::npos) {
    if (body == "+" || body == "-") {
      re = 0.0;
      im = (body[0] == '+') ? 1.0 : -1.0;
      return true;
    }
    double m = 0.0;
    if (!tiny_parse_part (body, 0, body.size (), m)) return false;
    re = 0.0;
    im = m;
    return true;
  }
  double r = 0.0, m = 0.0;
  if (!tiny_parse_part (body, 0, split, r)) return false;
  if (split + 1 == body.size ()) {
    m = 1.0;
  } else if (!tiny_parse_part (body, split, body.size (), m)) {
    return false;
  }
  re = r;
  im = m;
  return true;
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
  // rational?  a single '/', whole-digit numerator/denominator on both
  // sides (e.g. 4881175/2).  Cached libraries serialize exact ratios this
  // way, and strtod cannot consume them, so parse them before the real
  // branch; a denominator of zero or a malformed token stays a symbol.
  size_t slash = tok.find ('/', i);
  if (slash != std::string::npos && slash > i &&
      slash + 1 < tok.size () &&
      tok.find ('/', slash + 1) == std::string::npos) {
    bool ok = true;
    for (size_t k = i; ok && k < slash; k++)
      ok = (tok[k] >= '0' && tok[k] <= '9');
    for (size_t k = slash + 1; ok && k < tok.size (); k++)
      ok = (tok[k] >= '0' && tok[k] <= '9');
    if (ok) {
      long long num = 0;
      long long den = 0;
      for (size_t k = i; k < slash; k++)
        num = num * 10 + (tok[k] - '0');
      for (size_t k = slash + 1; k < tok.size (); k++)
        den = den * 10 + (tok[k] - '0');
      if (den != 0) {
        if (neg)
          num = -num;
        return gf::make_ratio (sc, (gf::int_) num, (gf::int_) den);
      }
    }
  }
  // real?  cached libraries serialize float constants (e.g. 1e-12, 2.5e3,
  // .5), so a token starting with a digit or '.' after an optional sign is
  // tried as a number and only accepted when strtod consumes the whole
  // token (a partial parse -- 1e, 1d3, 123abc -- stays a symbol).
  if (i < tok.size ()) {
    char c0= tok[i];
    if ((c0 >= '0' && c0 <= '9') || c0 == '.') {
      char* end= nullptr;
      double d= std::strtod (tok.c_str (), &end);
      if (end == tok.c_str () + static_cast<std::ptrdiff_t> (tok.size ()))
        return gf::make_real (sc, d);
    }
  }
  // Writer-printed numerics the branches above cannot take: rectangular
  // complex (3.0+3.0i, 0.0+2.0i, +inf.0+1.0i, 1+i, 2i, +i) and standalone
  // +inf.0/-inf.0/+nan.0. a+bi-shaped identifiers whose parts do not parse
  // (a+bi, hi) stay symbols, as do bare i, 3+4I, and 1@2 (the writer
  // normalizes polar to rectangular, so @ never reaches the cache).
  {
    double re = 0.0, im = 0.0;
    if (tiny_parse_complex (tok, re, im))
      return gf::make_complex (sc, re, im);
    size_t q = 0;
    if (q < tok.size () && (tok[q] == '+' || tok[q] == '-')) q++;
    std::string tail = tok.substr (q);
    if (tail == "inf.0" || tail == "nan.0") {
      double v = (tail[0] == 'i') ? std::numeric_limits<double>::infinity ()
                                  : std::numeric_limits<double>::quiet_NaN ();
      if (q > 0 && tok[0] == '-') v = -v;
      return gf::make_real (sc, v);
    }
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
tiny_read_form (gf::scheme* sc, gf::pointer port, TinyLabels& lab) {
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
    int head_loc = -1;   // gf::gc_protect loc once the list is allocated
    while (true) {
      tiny_skip_ws (sc, port);
      gf::int_ d = tiny_peek (sc, port);
      if (d < 0) {
        if (head_loc >= 0)
          gf::gc_unprotect_at (sc, head_loc);
        return gf::error (sc, gf::make_symbol (sc, "read-error"),
                         gf::list (sc, gf::make_string (sc, "unterminated list")));
      }
      if (d == close) {
        tiny_next (sc, port);
        if (head_loc >= 0)
          gf::gc_unprotect_at (sc, head_loc);
        return head;
      }
      gf::pointer el;
      if (d == '.') {
        // A '.' at an element position is a standalone dot (dotted pair)
        // iff the following char is a delimiter; otherwise it is the start
        // of a symbol token (e.g. the ellipsis `...'), which is read as an
        // ordinary element.
        tiny_next (sc, port);
        if (tiny_is_delim (tiny_peek (sc, port))) {
          if (first) {
            if (head_loc >= 0)
              gf::gc_unprotect_at (sc, head_loc);
            return gf::error (sc, gf::make_symbol (sc, "read-error"),
                             gf::list (sc, gf::make_string (sc, "dot with no element")));
          }
          gf::pointer b = tiny_read_form (sc, port, lab);
          tiny_skip_ws (sc, port);
          if (tiny_peek (sc, port) != close) {
            gf::gc_unprotect_at (sc, head_loc);
            return gf::error (sc, gf::make_symbol (sc, "read-error"),
                             gf::list (sc, gf::make_string (sc, "bad dotted pair")));
          }
          tiny_next (sc, port);
          gf::set_cdr (tail, b);
          gf::gc_unprotect_at (sc, head_loc);
          return head;
        }
        // not a standalone dot: '.' starts the token (already consumed);
        // read the rest and continue the list with it as an element.
        el = tiny_read_token (sc, port, '.');
      } else {
        el = tiny_read_form (sc, port, lab);
      }
      if (first) {
        head = gf::cons (sc, el, gf::nil (sc));
        tail = head;
        first = false;
        // The accumulating list is only reachable from this C++ frame; a GC
        // during a deep read (large cached records) would otherwise collect
        // it, returning dangling cells (<free cell!>).  Protect until done.
        head_loc = gf::gc_protect (sc, head);
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
                    gf::cons (sc, tiny_read_form (sc, port, lab), gf::nil (sc)));
  }
  if (c == '`') {
    tiny_next (sc, port);
    return gf::cons (sc, gf::make_symbol (sc, "quasiquote"),
                    gf::cons (sc, tiny_read_form (sc, port, lab), gf::nil (sc)));
  }
  if (c == ',') {
    tiny_next (sc, port);
    const char* name = "unquote";
    if (tiny_peek (sc, port) == '@') {
      tiny_next (sc, port);
      name = "unquote-splicing";
    }
    return gf::cons (sc, gf::make_symbol (sc, name),
                    gf::cons (sc, tiny_read_form (sc, port, lab), gf::nil (sc)));
  }
  if (c == '"')
    return tiny_read_string (sc, port);
  if (c == '|') {
    // Vertical-bar symbol: the writer escapes non-identifier symbols as
    // |...| (with \| and \\); escapes mirror strings via the shared core.
    // A | mid-token stays a token char (the full reader rejects foo|bar|
    // as invalid, so the cache never carries one).
    tiny_next (sc, port);  // consume |
    gf::pointer s = tiny_read_string_core (sc, port, '|');
    if (!gf::is_string (s)) return s;  // read-error, propagate
    return gf::make_symbol (sc, gf::string (s));
  }
  if (c == '#') {
    tiny_next (sc, port);
    gf::int_ d = tiny_peek (sc, port);
    if (d == 't') { tiny_next (sc, port); return gf::t (sc); }
    if (d == 'f') { tiny_next (sc, port); return gf::f (sc); }
    if (d == '\\') { tiny_next (sc, port); return tiny_read_char (sc, port); }
    if (d == '(') { return tiny_read_vector (sc, port, lab); }
    if (d >= '0' && d <= '9') {
      // Graph label (#n= def / #n# ref): the write-roundtrip graph pass
      // emits them for shared structure. Refs to an already-completed def
      // restore the alias; anything else (notably forward refs from cyclic
      // data, which the writer only emits with records) is a loud error
      // so the cache misses instead of corrupting.
      long n = 0;
      while (true) {
        gf::int_ h = tiny_peek (sc, port);
        if (h < '0' || h > '9') break;
        tiny_next (sc, port);
        n = n * 10 + (h - '0');
      }
      gf::int_ m = tiny_peek (sc, port);
      if (m == '=') {
        tiny_next (sc, port);
        gf::pointer obj = tiny_read_form (sc, port, lab);
        // An s7 error object is not a value: propagate, store nothing.
        if (gf::is_string (obj) || gf::is_pair (obj) || gf::is_vector (obj) ||
            gf::is_null (sc, obj) || gf::is_number (obj) || gf::is_boolean (obj) ||
            gf::is_symbol (obj) || gf::is_character (obj) || gf::is_keyword (obj) ||
            obj == gf::eof_object (sc) || obj == gf::unspecified (sc) ||
            obj == gf::undefined (sc))
          tiny_labels_store (sc, lab, (size_t) n, obj);
        return obj;
      }
      if (m == '#') {
        tiny_next (sc, port);
        if ((size_t) n < lab.objs.size () && lab.objs[(size_t) n] != nullptr)
          return lab.objs[(size_t) n];
        return gf::error (sc, gf::make_symbol (sc, "read-error"),
                         gf::list (sc, gf::make_string (sc, "undefined label")));
      }
      return gf::error (sc, gf::make_symbol (sc, "read-error"),
                       gf::list (sc, gf::make_string (sc, "bad # object")));
    }
    if (d == 'u') {
      // #u8( bytevector: the writer emits them, so the cache reader must
      // take them back (else every bytevector bundle misses). Elements
      // are writer-printed u8 integers; anything else is a loud error.
      tiny_next (sc, port);  // consume u
      if (tiny_peek (sc, port) != '8')
        return gf::error (sc, gf::make_symbol (sc, "read-error"),
                         gf::list (sc, gf::make_string (sc, "bad #u object")));
      tiny_next (sc, port);  // consume 8
      if (tiny_peek (sc, port) != '(')
        return gf::error (sc, gf::make_symbol (sc, "read-error"),
                         gf::list (sc, gf::make_string (sc, "bad #u8 object")));
      tiny_next (sc, port);  // consume (
      std::vector<gf::int_> els;
      while (true) {
        tiny_skip_ws (sc, port);
        gf::int_ e = tiny_peek (sc, port);
        if (e < 0)
          return gf::error (sc, gf::make_symbol (sc, "read-error"),
                           gf::list (sc, gf::make_string (sc, "unterminated bytevector")));
        if (e == ')') { tiny_next (sc, port); break; }
    gf::pointer el = tiny_read_form (sc, port, lab);
        if (!gf::is_integer (el) || gf::integer (el) < 0 || gf::integer (el) > 255)
          return gf::error (sc, gf::make_symbol (sc, "read-error"),
                           gf::list (sc, gf::make_string (sc, "bytevector element out of range")));
        els.push_back (gf::integer (el));
      }
      gf::pointer bv = gf::make_byte_vector (sc, (gf::int_) els.size (), 1, nullptr);
      int bloc = gf::gc_protect (sc, bv);
      for (size_t bi = 0; bi < els.size (); bi++)
        gf::byte_vector_set (bv, (gf::int_) bi, (uint8_t) els[bi]);      gf::gc_unprotect_at (sc, bloc);
      return bv;
    }
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
  TinyLabels lab;
  gf::pointer v = tiny_read_form (sc, port, lab);
  tiny_labels_release (sc, lab);
  return v;
}

static gf::pointer
f_tiny_read_with_default (gf::scheme* sc, gf::pointer args) {
  TinyLabels lab;
  gf::pointer v;
  if (gf::is_null (sc, args)) {
    gf::pointer ip = gf::current_input_port (sc);
    v = tiny_read_form (sc, ip, lab);
  } else {
    v = tiny_read_form (sc, gf::car (args), lab);
  }
  tiny_labels_release (sc, lab);
  return v;
}

static gf::pointer
tiny_load_path (gf::scheme* sc, const char* path) {
  gf::pointer port = gf::open_input_file (sc, path, "r");
  gf::pointer env = gf::rootlet (sc);
  gf::pointer result = gf::unspecified (sc);
  TinyLabels lab;
  while (true) {
    gf::pointer d = tiny_read_form (sc, port, lab);
    if (d == gf::eof_object (sc))
      break;
    result = gf::eval (sc, d, env);
  }
  tiny_labels_release (sc, lab);
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
  gf::define_function (sc, "g-undefined", f_undefined, 0, 1, false,
                      "(g-undefined [name]) => the #<undefined> object, or a named undefined #<name>");
  // replace S7's read with the tiny bootstrap read
  gf::define_function (sc, "read", f_tiny_read_with_default, 0, 1, false,
                      "(read [port]) => datum");
  // make-hook/call-with-values/etc. need `read` bound, so initialize them here
  gf::initialize_misc (sc);
}

} // namespace goldfish
