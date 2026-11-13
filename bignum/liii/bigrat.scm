;;; liii/bigrat.scm - 任意精度有理数 (<bigrat>) 基础模块
;;;
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

(define-library (liii bigrat)
  (import (scheme base) (liii bigint) (liii error))

  (export bigrat? make-bigrat bigrat-num bigrat-den bigrat->string
    bigrat-positive? bigrat-negative? bigrat=? bigrat<?
  ) ;export

  (begin
    (define-record-type <bigrat>
      (make-bigrat-raw num den)
      bigrat?
      (num bigrat-num)
      (den bigrat-den)
    ) ;define-record-type

    (define (to-bigint x)
      (if (bigint? x) x (integer->bigint x))
    ) ;define

    (define (make-bigrat num den)
      (let ((bn (to-bigint num)) (bd (to-bigint den)))
        (when (bigint-zero? bd)
          (value-error "make-bigrat in (liii bigrat): division by zero")
        ) ;when
        (let* ((neg-den? (bigint-negative? bd))
               (sn (if neg-den? (bigint-negate bn) bn))
               (sd (if neg-den? (bigint-negate bd) bd))
               (g (bigint-gcd (bigint-abs sn) sd))
              ) ;
          (make-bigrat-raw (bigint-quotient sn g) (bigint-quotient sd g))
        ) ;let*
      ) ;let
    ) ;define

    (define (bigrat->string r . rest)
      (if (null? rest)
        (string-append (bigint->string (bigrat-num r))
          "/"
          (bigint->string (bigrat-den r))
        ) ;string-append
        (let ((radix (car rest)))
          (string-append (bigint->string (bigrat-num r) radix)
            "/"
            (bigint->string (bigrat-den r) radix)
          ) ;string-append
        ) ;let
      ) ;if
    ) ;define

    (define (bigrat-positive? r)
      (bigint-positive? (bigrat-num r))
    ) ;define

    (define (bigrat-negative? r)
      (bigint-negative? (bigrat-num r))
    ) ;define

    (define (bigrat=? a b)
      (and (bigint=? (bigrat-num a) (bigrat-num b))
        (bigint=? (bigrat-den a) (bigrat-den b))
      ) ;and
    ) ;define

    (define (bigrat<? a b)
      (bigint<? (bigint-multiply (bigrat-num a) (bigrat-den b))
        (bigint-multiply (bigrat-num b) (bigrat-den a))
      ) ;bigint<?
    ) ;define
  ) ;begin
) ;define-library
