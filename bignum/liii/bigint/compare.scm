;;; liii/bigint/compare.scm - bigint 比较运算
;;;
;;; 负责 bigint 的顺序比较与相等比较。
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

(define-library (liii bigint compare)
  (import (scheme base) (liii error) (liii bigint record))

  (export bigint=? bigint<? bigint>? bigint<=? bigint>=?)

  (begin
    (define (ensure-bigint who obj)
      (if (bigint? obj)
        obj
        (type-error (string-append who " in (liii bigint compare): not a bigint") obj)
      ) ;if
    ) ;define

    (define (compare-mag a b)
      (let ((len-a (length a)) (len-b (length b)))
        (cond ((< len-a len-b) -1)
              ((> len-a len-b) 1)
              (else
                (let loop
                  ((a a) (b b))
                  (cond ((null? a) 0)
                        ((< (car a) (car b)) -1)
                        ((> (car a) (car b)) 1)
                        (else (loop (cdr a) (cdr b)))
                  ) ;cond
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (bigint-compare a b)
      (let ((signum-a (bigint-signum a)) (signum-b (bigint-signum b)))
        (cond ((< signum-a signum-b) -1)
              ((> signum-a signum-b) 1)
              ((= signum-a 0) 0)
              ((= signum-a 1) (compare-mag (bigint-mag a) (bigint-mag b)))
              (else (- (compare-mag (bigint-mag a) (bigint-mag b))))
        ) ;cond
      ) ;let
    ) ;define

    (define (bigint=? a b)
      (= (bigint-compare (ensure-bigint "bigint=?" a) (ensure-bigint "bigint=?" b)) 0)
    ) ;define

    (define (bigint<? a b)
      (< (bigint-compare (ensure-bigint "bigint<?" a) (ensure-bigint "bigint<?" b)) 0)
    ) ;define

    (define (bigint>? a b)
      (> (bigint-compare (ensure-bigint "bigint>?" a) (ensure-bigint "bigint>?" b)) 0)
    ) ;define

    (define (bigint<=? a b)
      (<= (bigint-compare (ensure-bigint "bigint<=?" a) (ensure-bigint "bigint<=?" b))
        0
      ) ;<=
    ) ;define

    (define (bigint>=? a b)
      (>= (bigint-compare (ensure-bigint "bigint>=?" a) (ensure-bigint "bigint>=?" b))
        0
      ) ;>=
    ) ;define

  ) ;begin
) ;define-library
