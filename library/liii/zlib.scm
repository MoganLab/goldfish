;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

;; (liii zlib) - gzip / deflate 压缩与解压（issue #1788）
;;
;; 一次性 API，语义对齐 Python zlib 模块：
;; - deflate-* 使用 zlib 容器（RFC 1950），gzip-* 使用 gzip 容器（RFC 1952），
;;   raw-deflate-* 使用裸 DEFLATE（RFC 1951，WS permessage-deflate 用）。
;; - decompress 自动识别 gzip / zlib 容器。
;; - string 输入按原始字节压缩；解压输出恒为 bytevector。

(define-library (liii zlib)
  (import (scheme base) (liii base))
  (export gzip-compress gzip-decompress deflate-compress deflate-decompress
    raw-deflate-compress raw-deflate-decompress decompress crc32 adler32
    zlib-version
  ) ;export
  (begin

    (define* (gzip-compress data
               (level -1)
               (window-bits 15)
               (strategy 'default)
               (mem-level 8)
             ) ;gzip-compress
      (g_zlib-gzip-compress data level window-bits strategy mem-level)
    ) ;define*

    (define* (deflate-compress data
               (level -1)
               (window-bits 15)
               (strategy 'default)
               (mem-level 8)
             ) ;deflate-compress
      (g_zlib-deflate-compress data level window-bits strategy mem-level)
    ) ;define*

    (define* (raw-deflate-compress data
               (level -1)
               (window-bits 15)
               (strategy 'default)
               (mem-level 8)
             ) ;raw-deflate-compress
      (g_zlib-raw-deflate-compress data level window-bits strategy mem-level)
    ) ;define*

    (define* (gzip-decompress bv (window-bits 15))
      (g_zlib-gzip-decompress bv window-bits)
    ) ;define*

    (define* (deflate-decompress bv (window-bits 15))
      (g_zlib-deflate-decompress bv window-bits)
    ) ;define*

    (define* (raw-deflate-decompress bv (window-bits 15))
      (g_zlib-raw-deflate-decompress bv window-bits)
    ) ;define*

    (define* (decompress bv (window-bits 15)) (g_zlib-decompress bv window-bits))

    (define* (crc32 bv (start 0)) (g_zlib-crc32 bv start))

    (define* (adler32 bv (start 1)) (g_zlib-adler32 bv start))

    (define (zlib-version)
      (g_zlib-version)
    ) ;define

  ) ;begin
) ;define-library
