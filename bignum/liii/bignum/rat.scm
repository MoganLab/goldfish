;;; liii/bignum/rat.scm - <bigrat> 数值塔适配与升降级（由 liii/bignum.scm include）
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

(define (bn:make-rat num den)
  (let ((bn (bn:to-bigint num)) (bd (bn:to-bigint den)))
    (when (bigint-zero? bd)
      (native:/ 1 0)
    ) ;when
    (if (bigint-zero? bn)
      0
      (let* ((r (make-bigrat bn bd)) (rn (bigrat-num r)) (rd (bigrat-den r)))
        (if (bigint=? rd bigint-one)
          (bn:demote-bigint rn)
          (let ((dn (bn:demote-bigint rn)) (dd (bn:demote-bigint rd)))
            (if (and (native:integer? dn) (native:integer? dd)) (native:/ dn dd) r)
          ) ;let
        ) ;if
      ) ;let*
    ) ;if
  ) ;let
) ;define

(define (bn:exact-rat-any? x)
  (or (bigint? x) (bigrat? x) (and (native:rational? x) (native:exact? x)))
) ;define

(define (bn:exact-rat-num x)
  (cond ((bigint? x) x)
        ((bigrat? x) (bigrat-num x))
        ((native:integer? x) (integer->bigint x))
        (else (integer->bigint (native:numerator x)))
  ) ;cond
) ;define

(define (bn:exact-rat-den x)
  (cond ((or (bigint? x) (native:integer? x)) bigint-one)
        ((bigrat? x) (bigrat-den x))
        (else (integer->bigint (native:denominator x)))
  ) ;cond
) ;define

(define (bn:bigrat->inexact r)
  (let* ((num (bigrat-num r))
         (den (bigrat-den r))
         (neg? (bigint-negative? num))
         (an (bigint-abs num))
         (shift (native:- (bigint-bit-length an) (bigint-bit-length den)))
        ) ;
    (cond ((native:> shift 1024) (if neg? -inf.0 +inf.0))
          ((native:< shift -1075) (if neg? -0.0 0.0))
          (else
            (let* ((k (native:- 60 shift))
                   (sn (if (native:> k 0) (bigint-multiply an (bigint-expt bigint-two k)) an))
                   (sd
                     (if (native:< k 0)
                       (bigint-multiply den (bigint-expt bigint-two (native:- k)))
                       den
                     ) ;if
                   ) ;sd
                   (q (bigint-quotient sn sd))
                   (mag (native:* (bn:bigint->inexact q) (native:expt 2.0 (native:- shift 60))))
                  ) ;
              (if neg? (native:- mag) mag)
            ) ;let*
          ) ;else
    ) ;cond
  ) ;let*
) ;define

(define (bn:bigrat-truncate r)
  (bn:demote-bigint (bigint-quotient (bigrat-num r) (bigrat-den r)))
) ;define

(define (bn:bigrat-floor r)
  (let-values (((q rem) (bigint-divide-and-remainder (bigrat-num r) (bigrat-den r))))
    (bn:demote-bigint (if (bigint-negative? rem) (bigint-subtract q bigint-one) q))
  ) ;let-values
) ;define

(define (bn:bigrat-ceiling r)
  (let-values (((q rem) (bigint-divide-and-remainder (bigrat-num r) (bigrat-den r))))
    (bn:demote-bigint (if (bigint-positive? rem) (bigint-add q bigint-one) q))
  ) ;let-values
) ;define

(define (bn:bigrat-round r)
  (let-values (((q rem) (bigint-divide-and-remainder (bigrat-num r) (bigrat-den r))))
    (let* ((den (bigrat-den r)) (two-r (bigint-multiply (bigint-abs rem) bigint-two)))
      (bn:demote-bigint
        (cond ((bigint<? two-r den) q)
              ((bigint>? two-r den)
               (if (bigint-negative? rem)
                 (bigint-subtract q bigint-one)
                 (bigint-add q bigint-one)
               ) ;if
              ) ;
              ((bigint-even? q) q)
              ((bigint-negative? rem) (bigint-subtract q bigint-one))
              (else (bigint-add q bigint-one))
        ) ;cond
      ) ;bn:demote-bigint
    ) ;let*
  ) ;let-values
) ;define
