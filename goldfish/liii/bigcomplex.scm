;;; liii/bigcomplex.scm - 任意精度复数 (<bigcomplex>) 基础模块
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

(define-library (liii bigcomplex)
  (import (scheme base) (liii bigint) (liii bigrat))

  (export bigcomplex? make-bigcomplex bigcomplex-real bigcomplex-imag
    bigcomplex->string
  ) ;export

  (begin
    (define-record-type <bigcomplex>
      (make-bigcomplex-raw real imag)
      bigcomplex?
      (real bigcomplex-real)
      (imag bigcomplex-imag)
    ) ;define-record-type

    (define (make-bigcomplex real imag)
      (make-bigcomplex-raw real imag)
    ) ;define

    (define (part->string x radix)
      (cond ((bigint? x) (bigint->string x radix))
            ((bigrat? x) (bigrat->string x radix))
            (else (number->string x radix))
      ) ;cond
    ) ;define

    (define (part-negative? x)
      (cond ((bigint? x) (bigint-negative? x))
            ((bigrat? x) (bigrat-negative? x))
            (else (< x 0))
      ) ;cond
    ) ;define

    (define (part-zero? x)
      (cond ((bigint? x) (bigint-zero? x))
            ((bigrat? x) #f)
            (else (= x 0))
      ) ;cond
    ) ;define

    (define (part-one? x)
      (and (not (bigint? x)) (not (bigrat? x)) (= x 1))
    ) ;define

    (define (part-neg-one? x)
      (and (not (bigint? x)) (not (bigrat? x)) (= x -1))
    ) ;define

    (define (bigcomplex->string z . rest)
      (let* ((radix (if (null? rest) 10 (car rest)))
             (r (bigcomplex-real z))
             (i (bigcomplex-imag z))
             (r-str (if (part-zero? r) "" (part->string r radix)))
             (i-str
               (cond ((part-one? i) "+i")
                     ((part-neg-one? i) "-i")
                     ((part-negative? i) (string-append (part->string i radix) "i"))
                     (else (string-append "+" (part->string i radix) "i"))
               ) ;cond
             ) ;i-str
            ) ;
        (string-append r-str i-str)
      ) ;let*
    ) ;define
  ) ;begin
) ;define-library
