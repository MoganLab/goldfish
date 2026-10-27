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

;; type-hint 规则的基础工具函数。

(define-library (liii goldcheck type-hint util)
  (import (scheme base) (liii string))
  (export join-strings split-slash strip-scm dedup sort-numbers syms-join)
  (begin

    (define (join-strings lst sep)
      (let loop
        ((l lst) (acc '()))
        (if (null? l)
          (apply string-append (reverse acc))
          (loop (cdr l) (if (null? acc) (cons (car l) acc) (cons (car l) (cons sep acc))))
        ) ;if
      ) ;let
    ) ;define

    (define (split-slash s)
      (let loop
        ((i 0) (start 0) (acc '()))
        (cond
         ((>= i (string-length s)) (reverse (cons (substring s start i) acc)))
         ((char=? (string-ref s i) #\/)
          (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))
         ) ;
         (else (loop (+ i 1) start acc))
        ) ;cond
      ) ;let
    ) ;define

    (define (strip-scm s)
      (if (string-ends? s ".scm") (substring s 0 (- (string-length s) 4)) s)
    ) ;define

    (define (dedup l)
      (let loop
        ((l l) (acc '()))
        (if (null? l)
          (reverse acc)
          (loop (cdr l) (if (memq (car l) acc) acc (cons (car l) acc)))
        ) ;if
      ) ;let
    ) ;define

    (define (sort-numbers l)
      (define (insert x a)
        (cond ((null? a) (list x))
              ((< x (car a)) (cons x a))
              (else (cons (car a) (insert x (cdr a))))
        ) ;cond
      ) ;define
      (let loop
        ((l l) (acc '()))
        (if (null? l) acc (loop (cdr l) (insert (car l) acc)))
      ) ;let
    ) ;define

    (define (syms-join syms)
      (join-strings (map symbol->string syms) " ")
    ) ;define

  ) ;begin
) ;define-library
