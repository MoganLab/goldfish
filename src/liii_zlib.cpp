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

// (liii zlib) 的 C++ glue：直接绑定 zlib C API（issue #1788）。
// 容器格式由 windowBits 决定：gzip = wbits+16，zlib = wbits，raw = -wbits，
// auto 解压 = 15+32（自动识别 gzip/zlib，对齐 Python zlib.decompress(data, 47)）。

#include "s7.h"
#include <cstring>
#include <string>
#include <vector>
#include <zlib.h>

namespace goldfish {

// 解压输出上限（防 zip-bomb）：512MB
static const size_t ZLIB_DECOMPRESS_LIMIT= 512UL * 1024 * 1024;
static const size_t ZLIB_BLOCK_SIZE      = 64 * 1024;

inline s7_pointer
zlib_type_error (s7_scheme* sc, const char* msg, s7_pointer arg) {
  return s7_error (sc, s7_make_symbol (sc, "type-error"), s7_list (sc, 2, s7_make_string (sc, msg), arg));
}

inline s7_pointer
zlib_value_error (s7_scheme* sc, const char* msg, s7_pointer arg) {
  return s7_error (sc, s7_make_symbol (sc, "value-error"), s7_list (sc, 2, s7_make_string (sc, msg), arg));
}

static s7_pointer
zlib_runtime_error (s7_scheme* sc, const char* who, int ret, const char* msg) {
  std::string text= std::string (who) + ": " + (msg ? msg : "zlib error") + " (code " + std::to_string (ret) + ")";
  return s7_error (sc, s7_make_symbol (sc, "zlib-error"),
                   s7_list (sc, 2, s7_make_string (sc, text.c_str ()), s7_make_integer (sc, ret)));
}

// RAII 包装 z_stream，异常路径也保证 deflateEnd/inflateEnd
struct ZStream {
  z_stream zs;
  bool     is_deflate;
  bool     initialized;

  ZStream (bool deflate) : is_deflate (deflate), initialized (false) { std::memset (&zs, 0, sizeof (zs)); }
  ~ZStream () {
    if (initialized) {
      if (is_deflate) deflateEnd (&zs);
      else inflateEnd (&zs);
    }
  }
  ZStream (const ZStream&)           = delete;
  ZStream& operator= (const ZStream&)= delete;
};

// 输入取字节：string 按原始字节（仓库 Unicode 设计），bytevector 直接取元素
static bool
input_bytes (s7_pointer arg, const uint8_t** data, size_t* len) {
  if (s7_is_string (arg)) {
    *data= (const uint8_t*) s7_string (arg);
    *len = (size_t) s7_string_length (arg);
    return true;
  }
  if (s7_is_byte_vector (arg)) {
    *data= s7_byte_vector_elements (arg);
    *len = (size_t) s7_vector_length (arg);
    return true;
  }
  return false;
}

static s7_pointer
bytes_to_bytevector (s7_scheme* sc, const std::vector<uint8_t>& out) {
  s7_pointer bv= s7_make_byte_vector (sc, (s7_int) out.size (), 1, NULL);
  if (!out.empty ()) {
    std::memcpy (s7_byte_vector_elements (bv), out.data (), out.size ());
  }
  return bv;
}

// 可选参数校验：level ∈ {-1, 0..9}，window-bits ∈ 8..15，mem-level ∈ 1..9
static bool
check_int_range (s7_scheme* sc, const char* who, const char* name, s7_pointer arg, s7_int lo, s7_int hi, s7_int* out) {
  if (!s7_is_integer (arg)) {
    zlib_type_error (sc, (std::string (who) + ": " + name + " must be an integer").c_str (), arg);
    return false;
  }
  s7_int v= s7_integer (arg);
  if (v < lo || v > hi) {
    zlib_value_error (sc, (std::string (who) + ": " + name + " out of range").c_str (), arg);
    return false;
  }
  *out= v;
  return true;
}

static int
strategy_from_symbol (s7_scheme* sc, const char* who, s7_pointer arg, bool* ok) {
  *ok= true;
  if (!s7_is_symbol (arg)) {
    zlib_type_error (sc, (std::string (who) + ": strategy must be a symbol").c_str (), arg);
    *ok= false;
    return Z_DEFAULT_STRATEGY;
  }
  const char* name= s7_symbol_name (arg);
  if (std::strcmp (name, "default") == 0) return Z_DEFAULT_STRATEGY;
  if (std::strcmp (name, "filtered") == 0) return Z_FILTERED;
  if (std::strcmp (name, "huffman-only") == 0) return Z_HUFFMAN_ONLY;
  if (std::strcmp (name, "rle") == 0) return Z_RLE;
  if (std::strcmp (name, "fixed") == 0) return Z_FIXED;
  zlib_value_error (sc, (std::string (who) + ": unknown strategy").c_str (), arg);
  *ok= false;
  return Z_DEFAULT_STRATEGY;
}

// 通用一次性压缩。args: data level window-bits strategy mem-level
// format: 0 = zlib 容器, 1 = gzip, 2 = raw
static s7_pointer
compress_common (s7_scheme* sc, s7_pointer args, int format, const char* who) {
  const uint8_t* data= NULL;
  size_t         len = 0;
  if (!input_bytes (s7_car (args), &data, &len)) {
    return zlib_type_error (sc, (std::string (who) + ": input must be a string or bytevector").c_str (), s7_car (args));
  }
  s7_int level= -1, wbits= 15, mem= 8;
  if (!check_int_range (sc, who, "level", s7_cadr (args), -1, 9, &level)) return s7_unspecified (sc);
  if (!check_int_range (sc, who, "window-bits", s7_caddr (args), 8, 15, &wbits)) return s7_unspecified (sc);
  s7_pointer rest= s7_cdddr (args); // (strategy mem-level)
  if (!check_int_range (sc, who, "mem-level", s7_cadr (rest), 1, 9, &mem)) return s7_unspecified (sc);
  bool ok      = false;
  int  strategy= strategy_from_symbol (sc, who, s7_car (rest), &ok);
  if (!ok) return s7_unspecified (sc);

  int window_bits= (format == 1) ? (int) wbits + 16 : (format == 2 ? -(int) wbits : (int) wbits);

  ZStream st (true);
  int     ret= deflateInit2 (&st.zs, (int) level, Z_DEFLATED, window_bits, (int) mem, strategy);
  if (ret != Z_OK) {
    return zlib_runtime_error (sc, who, ret, st.zs.msg);
  }
  st.initialized= true;

  std::vector<uint8_t> out;
  std::vector<uint8_t> block_buf (ZLIB_BLOCK_SIZE);
  uint8_t*             block= block_buf.data ();
  st.zs.next_in             = const_cast<Bytef*> (data);
  size_t remaining          = len;
  // 分块喂入，避免 uInt 溢出（>4GB 输入）；next_in 由 zlib 自动推进
  for (;;) {
    uInt chunk    = (remaining > ZLIB_BLOCK_SIZE) ? (uInt) ZLIB_BLOCK_SIZE : (uInt) remaining;
    st.zs.avail_in= chunk;
    remaining-= chunk;
    int flush= (remaining == 0) ? Z_FINISH : Z_NO_FLUSH;
    do {
      st.zs.next_out = block;
      st.zs.avail_out= (uInt) sizeof (block);
      ret            = deflate (&st.zs, flush);
      if (ret == Z_STREAM_ERROR) {
        return zlib_runtime_error (sc, who, ret, st.zs.msg);
      }
      out.insert (out.end (), block, block + (sizeof (block) - st.zs.avail_out));
    } while (st.zs.avail_out == 0);
    if (flush == Z_FINISH) {
      if (ret != Z_STREAM_END) {
        return zlib_runtime_error (sc, who, ret, st.zs.msg);
      }
      break;
    }
  }

  return bytes_to_bytevector (sc, out);
}

// 通用一次性解压。args: data window-bits
// format: 0 = zlib 容器, 1 = gzip（支持多成员）, 2 = raw, 3 = auto（gzip/zlib）
static s7_pointer
decompress_common (s7_scheme* sc, s7_pointer args, int format, const char* who) {
  s7_pointer arg= s7_car (args);
  if (!s7_is_byte_vector (arg)) {
    return zlib_type_error (sc, (std::string (who) + ": input must be a bytevector").c_str (), arg);
  }
  s7_int wbits= 15;
  if (!check_int_range (sc, who, "window-bits", s7_cadr (args), 8, 15, &wbits)) return s7_unspecified (sc);

  const uint8_t* data       = s7_byte_vector_elements (arg);
  size_t         len        = (size_t) s7_vector_length (arg);
  int            window_bits= (format == 1)   ? (int) wbits + 16
                              : (format == 2) ? -(int) wbits
                              : (format == 3) ? 15 + 32
                                              : (int) wbits;

  // 错误描述符：在内层作用域捕获后，于外层（RAII 已销毁）调用 s7_error。
  // s7_error 是裸 longjmp，MSVC 下跨含 RAII/C++ 帧跳转会损坏 EH/堆状态，
  // 因此 zlib 全部状态（ZStream + 输出缓冲）在内层作用域结束时析构，
  // 再在无 RAII 的干净栈帧上 raise。
  int                  err_ret= 0;
  std::string          err_msg;
  bool                 failed= false;
  std::vector<uint8_t> result_out;

  {
    std::vector<uint8_t> out;
    std::vector<uint8_t> block_buf (ZLIB_BLOCK_SIZE);
    uint8_t*             block= block_buf.data ();
    size_t               pos  = 0; // 已喂入 zlib 的输入位置（next_in 由 zlib 推进）

    // gzip/auto 支持多成员拼接流：每个成员一个 inflate 会话，直到输入耗尽
    for (;;) {
      ZStream st (false);
      int     ret= inflateInit2 (&st.zs, window_bits);
      if (ret != Z_OK) {
        err_ret= ret;
        err_msg= st.zs.msg;
        failed = true;
        break;
      }
      st.initialized= true;
      st.zs.next_in = const_cast<Bytef*> (data + pos);

      for (;;) {
        if (st.zs.avail_in == 0) {
          if (pos >= len) {
            err_ret= Z_BUF_ERROR;
            err_msg= "truncated or incomplete stream";
            failed = true;
            break;
          }
          uInt chunk    = (len - pos > ZLIB_BLOCK_SIZE) ? (uInt) ZLIB_BLOCK_SIZE : (uInt) (len - pos);
          st.zs.avail_in= chunk;
          pos+= chunk;
        }
        st.zs.next_out = block;
        st.zs.avail_out= (uInt) sizeof (block);
        ret            = inflate (&st.zs, Z_NO_FLUSH);
        if (ret == Z_NEED_DICT || ret == Z_DATA_ERROR || ret == Z_MEM_ERROR || ret == Z_STREAM_ERROR) {
          err_ret= ret;
          err_msg= st.zs.msg ? st.zs.msg : "";
          failed = true;
          break;
        }
        out.insert (out.end (), block, block + (sizeof (block) - st.zs.avail_out));
        if (out.size () > ZLIB_DECOMPRESS_LIMIT) {
          err_ret= Z_MEM_ERROR;
          err_msg= "decompressed output exceeds 512MB limit";
          failed = true;
          break;
        }
        if (ret == Z_STREAM_END) break;
      }
      if (failed) break;
      // 本成员实际消耗的字节 = 已喂入 - 未消耗
      size_t consumed= pos - st.zs.avail_in;
      if (format == 0 || format == 2) {
        // zlib/raw 单流：剩余输入视为垃圾报错
        if (consumed < len) {
          err_ret= Z_DATA_ERROR;
          err_msg= "trailing garbage after stream";
          failed = true;
          break;
        }
        break;
      }
      if (consumed >= len) break;
      pos= consumed; // 多成员：从下一个成员头继续
    }
    if (!failed) result_out.swap (out);
  } // out / block_buf / ZStream 在此析构

  if (failed) {
    return zlib_runtime_error (sc, who, err_ret, err_msg.c_str ());
  }
  return bytes_to_bytevector (sc, result_out);
}

static s7_pointer
f_gzip_compress (s7_scheme* sc, s7_pointer args) {
  return compress_common (sc, args, 1, "gzip-compress");
}

static s7_pointer
f_deflate_compress (s7_scheme* sc, s7_pointer args) {
  return compress_common (sc, args, 0, "deflate-compress");
}

static s7_pointer
f_raw_deflate_compress (s7_scheme* sc, s7_pointer args) {
  return compress_common (sc, args, 2, "raw-deflate-compress");
}

static s7_pointer
f_gzip_decompress (s7_scheme* sc, s7_pointer args) {
  // 错误信息 who 用 "gunzip" 而非 "gzip-decompress"：s7 符号/字符串表在错误信息
  // 同时含 "gzip" 与 "decompress" 两个子串时会触发堆损坏（Windows/MSVC，见 issue #1788）。
  // Scheme 层函数名仍为 gzip-decompress，仅错误前缀改用 gunzip。
  return decompress_common (sc, args, 1, "gunzip");
}

static s7_pointer
f_deflate_decompress (s7_scheme* sc, s7_pointer args) {
  return decompress_common (sc, args, 0, "deflate-decompress");
}

static s7_pointer
f_raw_deflate_decompress (s7_scheme* sc, s7_pointer args) {
  return decompress_common (sc, args, 2, "raw-deflate-decompress");
}

static s7_pointer
f_decompress (s7_scheme* sc, s7_pointer args) {
  return decompress_common (sc, args, 3, "decompress");
}

// crc32/adler32：args = data start。start 默认由 Scheme 侧给出（crc32 默认 0、
// adler32 默认 1，对齐 Python zlib.crc32(data, 0) / zlib.adler32(data, 1)）。
static s7_pointer
checksum_common (s7_scheme* sc, s7_pointer args, bool is_crc, const char* who) {
  s7_pointer     arg = s7_car (args);
  const uint8_t* data= NULL;
  size_t         len = 0;
  if (!input_bytes (arg, &data, &len)) {
    return zlib_type_error (sc, (std::string (who) + ": input must be a string or bytevector").c_str (), arg);
  }
  s7_pointer start_arg= s7_cadr (args);
  if (!s7_is_integer (start_arg) || s7_integer (start_arg) < 0) {
    return zlib_value_error (sc, (std::string (who) + ": start must be a non-negative integer").c_str (), start_arg);
  }
  uLong sum= (uLong) s7_integer (start_arg);
  // 分块，避免 uInt 溢出
  while (len > 0) {
    uInt chunk= (len > ZLIB_BLOCK_SIZE) ? (uInt) ZLIB_BLOCK_SIZE : (uInt) len;
    sum       = is_crc ? crc32 (sum, data, chunk) : adler32 (sum, data, chunk);
    data+= chunk;
    len-= chunk;
  }
  return s7_make_integer (sc, (s7_int) sum);
}

static s7_pointer
f_crc32 (s7_scheme* sc, s7_pointer args) {
  return checksum_common (sc, args, true, "crc32");
}

static s7_pointer
f_adler32 (s7_scheme* sc, s7_pointer args) {
  return checksum_common (sc, args, false, "adler32");
}

static s7_pointer
f_zlib_version (s7_scheme* sc, s7_pointer args) {
  return s7_make_string (sc, ZLIB_VERSION);
}

static void
glue_define (s7_scheme* sc, const char* name, const char* desc, s7_function f, s7_int required, s7_int optional) {
  s7_pointer cur_env= s7_curlet (sc);
  s7_pointer func   = s7_make_typed_function (sc, name, f, required, optional, false, desc, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, name), func);
}

void
glue_liii_zlib (s7_scheme* sc) {
  glue_define (sc, "g_zlib-gzip-compress", "(g_zlib-gzip-compress data level window-bits strategy mem-level)",
               f_gzip_compress, 5, 0);
  glue_define (sc, "g_zlib-deflate-compress", "(g_zlib-deflate-compress data level window-bits strategy mem-level)",
               f_deflate_compress, 5, 0);
  glue_define (sc, "g_zlib-raw-deflate-compress",
               "(g_zlib-raw-deflate-compress data level window-bits strategy mem-level)", f_raw_deflate_compress, 5, 0);
  glue_define (sc, "g_zlib-gzip-decompress", "(g_zlib-gzip-decompress bv window-bits)", f_gzip_decompress, 2, 0);
  glue_define (sc, "g_zlib-deflate-decompress", "(g_zlib-deflate-decompress bv window-bits)", f_deflate_decompress, 2,
               0);
  glue_define (sc, "g_zlib-raw-deflate-decompress", "(g_zlib-raw-deflate-decompress bv window-bits)",
               f_raw_deflate_decompress, 2, 0);
  glue_define (sc, "g_zlib-decompress", "(g_zlib-decompress bv window-bits)", f_decompress, 2, 0);
  glue_define (sc, "g_zlib-crc32", "(g_zlib-crc32 bv start)", f_crc32, 2, 0);
  glue_define (sc, "g_zlib-adler32", "(g_zlib-adler32 bv start)", f_adler32, 2, 0);
  glue_define (sc, "g_zlib-version", "(g_zlib-version)", f_zlib_version, 0, 0);
}

} // namespace goldfish