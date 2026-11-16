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

(define-library (liii syntax)
  (export quote-form? unquote-form? make-syntactic-closure syntactic-closure?
    syntactic-closure-env syntactic-closure-expr syntactic-closure-free-vars
    syntactic-closure-rename syntactic-closure-set-rename! identifier?
    identifier->symbol identifier=? strip-syntactic-closures
    resolve-syntactic-closures
  ) ;export
  (import (scheme base))
  (begin
    ;; ; quote 形式：(quote x)。
    ;; ; [1292] 开启 (*s7* 'symbol-quote?) 后，reader 将 'x 读为 (quote x)，
    ;; ; 不再产生旧的 #_quote 语法对象形式。
    (define (quote-form? x)
      (and (pair? x) (eq? (car x) 'quote) (pair? (cdr x)) (null? (cddr x)))
    ) ;define

    ;; ; unquote 形式：(unquote x) 或 (unquote-splicing x)。
    ;; ; s7 reader 把 ,x 读为 (unquote x)，unquote 是普通符号；
    ;; ; unquote-splicing 用于规范化后的 splicing 形式。
    (define (unquote-form? x)
      (and (pair? x)
        (or (eq? (car x) 'unquote) (eq? (car x) 'unquote-splicing))
        (pair? (cdr x))
        (null? (cddr x))
      ) ;and
    ) ;define
  ) ;begin
) ;define-library
