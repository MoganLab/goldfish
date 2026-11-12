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

;; (hint scheme char) - (scheme char) 的类型接口

(define-library (hint scheme char)
  (import (scheme base))
  (export hint-scheme/char)
  (begin

    ;; 大小写转换
    (define hint-char-upcase '((char? -> char?)))
    (define hint-char-downcase '((char? -> char?)))
    (define hint-char-foldcase '((char? -> char?)))

    ;; 大小写判定
    (define hint-char-upper-case? '((char? -> boolean?)))
    (define hint-char-lower-case? '((char? -> boolean?)))

    ;; 字符分类与数值
    (define hint-digit-value '((char? -> (or #f integer?))))
    (define hint-char-numeric? '((char? -> boolean?)))
    (define hint-char-alphabetic? '((char? -> boolean?)))
    (define hint-char-whitespace? '((char? -> boolean?)))

    ;; 忽略大小写的字符比较：至少两个字符，可继续追加
    (define hint-char-ci=?
      '((char? char? -> boolean?) (char? char? char? -> boolean?))
    ) ;define
    (define hint-char-ci<?
      '((char? char? -> boolean?) (char? char? char? -> boolean?))
    ) ;define
    (define hint-char-ci>?
      '((char? char? -> boolean?) (char? char? char? -> boolean?))
    ) ;define
    (define hint-char-ci<=?
      '((char? char? -> boolean?) (char? char? char? -> boolean?))
    ) ;define
    (define hint-char-ci>=?
      '((char? char? -> boolean?) (char? char? char? -> boolean?))
    ) ;define

    ;; 忽略大小写的字符串比较：至少两个字符串，可继续追加
    (define hint-string-ci=?
      '((string? string? -> boolean?) (string? string? string? -> boolean?))
    ) ;define
    (define hint-string-ci<?
      '((string? string? -> boolean?) (string? string? string? -> boolean?))
    ) ;define
    (define hint-string-ci>?
      '((string? string? -> boolean?) (string? string? string? -> boolean?))
    ) ;define
    (define hint-string-ci<=?
      '((string? string? -> boolean?) (string? string? string? -> boolean?))
    ) ;define
    (define hint-string-ci>=?
      '((string? string? -> boolean?) (string? string? string? -> boolean?))
    ) ;define

    ;; 字符串大小写转换
    (define hint-string-upcase '((string? -> string?)))
    (define hint-string-downcase '((string? -> string?)))
    (define hint-string-foldcase '((string? -> string?)))

    (define (hint-scheme/char name)
      (case name
       ((char-upcase) hint-char-upcase)
       ((char-downcase) hint-char-downcase)
       ((char-foldcase) hint-char-foldcase)
       ((char-upper-case?) hint-char-upper-case?)
       ((char-lower-case?) hint-char-lower-case?)
       ((digit-value) hint-digit-value)
       ((char-numeric?) hint-char-numeric?)
       ((char-alphabetic?) hint-char-alphabetic?)
       ((char-whitespace?) hint-char-whitespace?)
       ((char-ci=?) hint-char-ci=?)
       ((char-ci<?) hint-char-ci<?)
       ((char-ci>?) hint-char-ci>?)
       ((char-ci<=?) hint-char-ci<=?)
       ((char-ci>=?) hint-char-ci>=?)
       ((string-ci=?) hint-string-ci=?)
       ((string-ci<?) hint-string-ci<?)
       ((string-ci>?) hint-string-ci>?)
       ((string-ci<=?) hint-string-ci<=?)
       ((string-ci>=?) hint-string-ci>=?)
       ((string-upcase) hint-string-upcase)
       ((string-downcase) hint-string-downcase)
       ((string-foldcase) hint-string-foldcase)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
