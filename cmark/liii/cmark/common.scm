;;;
;;; cmark 公共工具（被多个行内模块共享，避免相互 import 循环）
;;;
;;
;; Copyright (C) 2026 Liii Network
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
(define-library (liii cmark common)
  (import (scheme base))
  (import (scheme char))
  (import (liii cmark node))
  (import (liii cmark text))
  (import (liii cmark inline escape))
  (import (liii cmark inline entity))
  (export percent-encode-uri)
  (export decode-title-escapes)
  (export node-data-ref)
  (export html-tag-name-char?)
  (export read-html-tag-name)
  (export comment-start?)
  (export processing-instruction-start?)
  (export cdata-start?)
  (export declaration-start?)
  (begin

    ;; cmark 节点 data alist 取 key（带默认值）（node-data-ref 三份统一版，见 1027 A7）
    (define (node-data-ref node key default)
      (assoc-ref-default (cmark-node-data node) key default)
    ) ;define

    ;; title 解码反斜杠转义与字符引用（decode-link-title / decode-title-escapes 统一版，见 1027 A8）
    (define (decode-title-escapes str)
      (decode-backslash-escapes-in-string (decode-entity-references-in-string str))
    ) ;define

    ;; RFC 3986 unreserved + reserved 字符集（不需百分号编码）
    ;; extra-encode-chars 为额外强制编码的字符（autolink 场景需编码 []，CommonMark 要求）
    (define (url-unreserved-or-reserved? c extra-encode-chars)
      (let ((b (char->integer c)))
        (and (not (memv c extra-encode-chars))
          (or (and (>= b 65) (<= b 90))
            (and (>= b 97) (<= b 122))
            (and (>= b 48) (<= b 57))
            (string-index "-._~:/?#[]@!$&'()*+,;=%" c 0)
          ) ;or
        ) ;and
      ) ;let
    ) ;define

    ;; URL 百分号编码: 保留上述字符，其余字节 %XX（CommonMark Example 20/32/33/195/202/206）
    ;; 可选 extra-encode-chars 参数强制额外编码字符（如 autolink 的 #\[ #\]）
    (define (percent-encode-uri str . extra-encode-chars)
      (define (byte->hex b)
        (let ((h (string-upcase (number->string b 16))))
          (if (= (string-length h) 1) (string-append "0" h) h)
        ) ;let
      ) ;define
      (let ((len (string-length str)))
        (let loop
          ((pos 0) (parts '()))
          (if (>= pos len)
            (string-join (reverse parts) "")
            (let ((c (string-ref str pos)))
              (if (url-unreserved-or-reserved? c extra-encode-chars)
                (loop (+ pos 1) (cons (string c) parts))
                (loop (+ pos 1) (cons (string-append "%" (byte->hex (char->integer c))) parts))
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; ---- HTML 公共工具（inline/block 两个 html util 共享，见 1027 A6）----

    ;; HTML 标签名字符：字母 / 数字 / 连字符
    (define (html-tag-name-char? ch)
      (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\-))
    ) ;define

    ;; 从 start 读 HTML 标签名，返回名字结束位置（或 #f）；名字须以字母开头
    (define (read-html-tag-name str start)
      (let ((len (string-length str)))
        (if
          (or (>= start len) (not (char-alphabetic? (string-ref str start))))
          #f
          (let loop
            ((i (+ start 1)))
            (if (and (< i len) (html-tag-name-char? (string-ref str i))) (loop (+ i 1)) i)
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (comment-start? str pos)
      (string-prefix-at? str pos "<!--")
    ) ;define
    (define (processing-instruction-start? str pos)
      (string-prefix-at? str pos "<?")
    ) ;define
    (define (cdata-start? str pos)
      (string-prefix-at? str pos "<![CDATA[")
    ) ;define
    (define (declaration-start? str pos)
      (and (string-prefix-at? str pos "<!")
        (not (comment-start? str pos))
        (not (cdata-start? str pos))
        (< (+ pos 2) (string-length str))
        (char-alphabetic? (string-ref str (+ pos 2)))
      ) ;and
    ) ;define
  ) ;begin
) ;define-library
