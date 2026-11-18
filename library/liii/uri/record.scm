;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;  http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;; ; (liii uri record) - URI 记录构造、组件访问、编码与查询字符串子库
;; ; 函数文档见 bin/gf doc liii/uri 与 library/tests/liii/uri/record/README.md

(define-library (liii uri record)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii list)
    (liii error)
    (liii unicode)
    (liii uri internal record)
    (liii uri internal parse)
  ) ;import

  ;; ; ---------- 导出接口 ----------
  ;; 构造 URI
  (export make-uri-raw)

  ;; 谓词
  (export uri?)

  ;; 访问 URI 组件
  (export uri-scheme uri-raw-scheme)
  (export uri-host uri-raw-host uri-port uri-explicit-port)
  (export uri-user uri-raw-user uri-password uri-raw-password)
  (export uri-authority uri-raw-authority)
  (export uri-path uri-raw-path uri-path->list)
  (export uri-query uri-query-string uri-query-ref uri-query-ref*)
  (export uri-fragment uri-raw-fragment)
  (export uri-parent uri-name uri-suffix uri-suffixes)

  ;; 转换与编码
  (export uri-encode uri-decode)
  (export uri-encode-path uri-decode-path)
  (export query-string->alist alist->query-string)

  ;; 错误
  (export uri-error)

  (begin

    ;; ; ---------- 错误条件类型 ----------
    (define (uri-error . args)
      (apply error 'uri-error args)
    ) ;define

    ;; ; ---------- 记录构造与谓词 ----------
    (define (make-uri-raw scheme netloc path query fragment)
      (%make-uri-record scheme netloc path query fragment)
    ) ;define

    (define (uri? obj)
      (%uri? obj)
    ) ;define

    ;; ; ---------- 编码与解码 ----------
    (define UNRESERVED_CHARS
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~"
    ) ;define

    (define (unreserved-byte? b)
      (and (>= b 0)
        (<= b 127)
        (string-contains UNRESERVED_CHARS (string (integer->char b)))
      ) ;and
    ) ;define

    (define (hex-digit n)
      (if (< n 10)
        (integer->char (+ n (char->integer #\0)))
        (integer->char (+ (- n 10) (char->integer #\A)))
      ) ;if
    ) ;define

    (define (char->hex c)
      (let ((code (char->integer c)))
        (cond
         ((and (>= code (char->integer #\0)) (<= code (char->integer #\9)))
          (- code (char->integer #\0))
         ) ;
         ((and (>= code (char->integer #\A)) (<= code (char->integer #\F)))
          (+ 10 (- code (char->integer #\A)))
         ) ;
         ((and (>= code (char->integer #\a)) (<= code (char->integer #\f)))
          (+ 10 (- code (char->integer #\a)))
         ) ;
         (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (bytevector-fold-encode bv encode-fn)
      (let ((len (bytevector-length bv)))
        (let loop
          ((i 0) (result '()))
          (if (>= i len)
            (list->string (reverse result))
            (loop (+ i 1) (encode-fn (bytevector-u8-ref bv i) result))
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (uri-encode str)
      (if (not (string? str))
        (type-error "uri-encode: expected string")
        (let ((bv (string->utf8 str)))
          (bytevector-fold-encode bv
            (lambda (b result)
              (if (unreserved-byte? b)
                (cons (integer->char b) result)
                (let ((hi (hex-digit (quotient b 16))) (lo (hex-digit (remainder b 16))))
                  (cons lo (cons hi (cons #\% result)))
                ) ;let
              ) ;if
            ) ;lambda
          ) ;bytevector-fold-encode
        ) ;let
      ) ;if
    ) ;define

    (define (uri-decode str)
      (if (not (string? str))
        (type-error "uri-decode: expected string")
        (let loop
          ((chars (string->list str)) (result '()))
          (if (null? chars)
            (list->string (reverse result))
            (let ((c (car chars)))
              (cond
               ((char=? c #\%)
                (if
                  (and (not (null? (cdr chars)))
                    (not (null? (cddr chars)))
                    (char->hex (cadr chars))
                    (char->hex (caddr chars))
                  ) ;and
                  (let ((high (char->hex (cadr chars))) (low (char->hex (caddr chars))))
                    (loop (cdddr chars) (cons (integer->char (+ (* high 16) low)) result))
                  ) ;let
                  (error "uri-decode: invalid percent encoding")
                ) ;if
               ) ;
               ((char=? c #\+) (loop (cdr chars) (cons #\space result)))
               (else (loop (cdr chars) (cons c result)))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (uri-encode-path path)
      (if (not (string? path))
        (error "uri-encode-path: expected string")
        (let ((bv (string->utf8 path)))
          (bytevector-fold-encode bv
            (lambda (b result)
              (cond ((= b 47) (cons #\/ result))
                    ((unreserved-byte? b) (cons (integer->char b) result))
                    (else
                      (let ((hi (hex-digit (quotient b 16))) (lo (hex-digit (remainder b 16))))
                        (cons lo (cons hi (cons #\% result)))
                      ) ;let
                    ) ;else
              ) ;cond
            ) ;lambda
          ) ;bytevector-fold-encode
        ) ;let
      ) ;if
    ) ;define

    (define (uri-decode-path path)
      (uri-decode path)
    ) ;define

    ;; ; ---------- 查询字符串 ----------
    (define (query-string->alist qs)
      (if (or (not (string? qs)) (string=? qs ""))
        '()
        (let ((pairs (string-split qs "&")))
          (map
            (lambda (pair)
              (let ((eq-pos (string-index pair #\=)))
                (if eq-pos
                  (cons (substring pair 0 eq-pos)
                    (uri-decode (substring pair (+ eq-pos 1) (string-length pair)))
                  ) ;cons
                  (cons pair "")
                ) ;if
              ) ;let
            ) ;lambda
            pairs
          ) ;map
        ) ;let
      ) ;if
    ) ;define

    (define (alist->query-string alist)
      (if (null? alist)
        ""
        (string-join
          (map
            (lambda (pair)
              (if (cdr pair)
                (string-append (car pair) "=" (uri-encode (cdr pair)))
                (car pair)
              ) ;if
            ) ;lambda
            alist
          ) ;map
          "&"
        ) ;string-join
      ) ;if
    ) ;define

    ;; ; ---------- scheme/host/port 访问器 ----------
    (define (uri-scheme uri-obj)
      (%uri-record-scheme uri-obj)
    ) ;define

    (define (uri-raw-scheme uri-obj)
      (%uri-record-scheme uri-obj)
    ) ;define

    (define (uri-host uri-obj)
      (let ((netloc-parts (parse-netloc (%uri-record-netloc uri-obj))))
        (list-ref netloc-parts 2)
      ) ;let
    ) ;define

    (define (uri-raw-host uri-obj)
      (uri-host uri-obj)
    ) ;define

    (define (uri-port uri-obj)
      (let* ((netloc-parts (parse-netloc (%uri-record-netloc uri-obj)))
             (explicit-port (list-ref netloc-parts 3))
             (scheme (%uri-record-scheme uri-obj))
            ) ;
        (or explicit-port (and scheme (%uri-default-port scheme)) #f)
      ) ;let*
    ) ;define

    (define (uri-explicit-port uri-obj)
      (let ((netloc-parts (parse-netloc (%uri-record-netloc uri-obj))))
        (list-ref netloc-parts 3)
      ) ;let
    ) ;define

    ;; ; ---------- authority/user/password 访问器 ----------
    (define (uri-user uri-obj)
      (let ((netloc-parts (parse-netloc (%uri-record-netloc uri-obj))))
        (list-ref netloc-parts 0)
      ) ;let
    ) ;define

    (define (uri-raw-user uri-obj)
      (uri-user uri-obj)
    ) ;define

    (define (uri-password uri-obj)
      (let ((netloc-parts (parse-netloc (%uri-record-netloc uri-obj))))
        (list-ref netloc-parts 1)
      ) ;let
    ) ;define

    (define (uri-raw-password uri-obj)
      (uri-password uri-obj)
    ) ;define

    (define (uri-authority uri-obj)
      (%uri-record-netloc uri-obj)
    ) ;define

    (define (uri-raw-authority uri-obj)
      (%uri-record-netloc uri-obj)
    ) ;define

    ;; ; ---------- path 访问器 ----------
    (define (uri-path uri-obj)
      (%uri-record-path uri-obj)
    ) ;define

    (define (uri-raw-path uri-obj)
      (%uri-record-path uri-obj)
    ) ;define

    (define (uri-path->list uri-obj)
      (let ((path (%uri-record-path uri-obj)))
        (if (or (not path) (string=? path "") (string=? path "/"))
          '()
          (let ((segments (string-split path "/")))
            (if (string=? (car segments) "") (cdr segments) segments)
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; ; ---------- query 访问器 ----------
    (define (uri-query uri-obj)
      (%uri-record-query uri-obj)
    ) ;define

    (define (uri-query-string uri-obj)
      (alist->query-string (%uri-record-query uri-obj))
    ) ;define

    (define (uri-query-ref uri-obj key)
      (let ((query (%uri-record-query uri-obj)))
        (let ((pair (assoc key query)))
          (if pair (cdr pair) #f)
        ) ;let
      ) ;let
    ) ;define

    (define (uri-query-ref* uri-obj key . rest)
      (let ((default (if (null? rest) #f (car rest))) (query (%uri-record-query uri-obj)))
        (let ((pair (assoc key query)))
          (if pair (cdr pair) default)
        ) ;let
      ) ;let
    ) ;define

    ;; ; ---------- fragment 访问器 ----------
    (define (uri-fragment uri-obj)
      (%uri-record-fragment uri-obj)
    ) ;define

    (define (uri-raw-fragment uri-obj)
      (%uri-record-fragment uri-obj)
    ) ;define

    ;; ; ---------- 路径相关访问器 ----------
    (define (uri-parent uri-obj)
      (let ((path (%uri-record-path uri-obj)))
        (if (or (not path) (string=? path "") (string=? path "/"))
          #f
          (let ((last-slash (string-index-right path #\/)))
            (if last-slash (substring path 0 last-slash) "")
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (uri-name uri-obj)
      (let ((path (%uri-record-path uri-obj)))
        (if (or (not path) (string=? path ""))
          #f
          (let ((last-slash (string-index-right path #\/)))
            (if last-slash (substring path (+ last-slash 1) (string-length path)) path)
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (uri-suffix uri-obj)
      (let ((name (uri-name uri-obj)))
        (if name
          (let ((last-dot (string-index-right name #\.)))
            (if last-dot (substring name (+ last-dot 1) (string-length name)) #f)
          ) ;let
          #f
        ) ;if
      ) ;let
    ) ;define

    (define (uri-suffixes uri-obj)
      (let ((name (uri-name uri-obj)))
        (if name
          (let ((parts (string-split name ".")))
            (if (> (length parts) 1) (cdr parts) '())
          ) ;let
          '()
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
