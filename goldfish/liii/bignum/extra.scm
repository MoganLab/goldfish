;;; liii/bignum/extra.scm - (scheme base)/(scheme inexact)/(scheme complex) 外围过程适配（由 liii/bignum.scm include）
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

(define (bn:square x)
  (bn:mul-2 x x)
) ;define

(define (bn:exact-integer-sqrt n)
  (if (bigint? n)
    (let-values (((s r) (bigint-sqrt-and-remainder n)))
      (values (bn:demote-bigint s) (bn:demote-bigint r))
    ) ;let-values
    (native:exact-integer-sqrt n)
  ) ;if
) ;define

(define (bn:floor-quotient x y)
  (bn:floor (bn:div-2 x y))
) ;define

(define (bn:floor-remainder x y)
  (bn:sub-2 x (bn:mul-2 y (bn:floor-quotient x y)))
) ;define

(define (bn:floor/ x y)
  (let ((q (bn:floor-quotient x y)))
    (values q (bn:sub-2 x (bn:mul-2 y q)))
  ) ;let
) ;define

(define (bn:truncate-quotient x y)
  (bn:truncate (bn:div-2 x y))
) ;define

(define (bn:truncate-remainder x y)
  (bn:sub-2 x (bn:mul-2 y (bn:truncate-quotient x y)))
) ;define

(define (bn:truncate/ x y)
  (let ((q (bn:truncate-quotient x y)))
    (values q (bn:sub-2 x (bn:mul-2 y q)))
  ) ;let
) ;define

(define (bn:sqrt-exact-int n)
  (let-values (((s r) (bigint-sqrt-and-remainder (bn:to-bigint n))))
    (if (bigint-zero? r) (bn:demote-bigint s) #f)
  ) ;let-values
) ;define

(define (bn:sqrt x)
  (cond
   ((bigint? x)
    (if (bigint-negative? x)
      (let ((sq (bn:sqrt-exact-int (bigint-negate x))))
        (if sq (bn:make-complex 0 sq) (native:sqrt (bn:bigint->inexact x)))
      ) ;let
      (or (bn:sqrt-exact-int x) (native:sqrt (bn:bigint->inexact x)))
    ) ;if
   ) ;
   ((bigrat? x)
    (if (bigrat-negative? x)
      (native:sqrt (bn:bigrat->inexact x))
      (let ((sn (bn:sqrt-exact-int (bigrat-num x)))
            (sd (bn:sqrt-exact-int (bigrat-den x)))
           ) ;
        (if (and sn sd) (bn:make-rat sn sd) (native:sqrt (bn:bigrat->inexact x)))
      ) ;let
    ) ;if
   ) ;
   ((bigcomplex? x) (native:sqrt (bn:bigcomplex->inexact x)))
   (else (native:sqrt x))
  ) ;cond
) ;define

(define (bn:magnitude x)
  (cond ((or (bigint? x) (bigrat? x)) (bn:abs x))
        ((bigcomplex? x)
         (let ((r (bigcomplex-real x)) (i (bigcomplex-imag x)))
           (bn:sqrt (bn:add-2 (bn:mul-2 r r) (bn:mul-2 i i)))
         ) ;let
        ) ;
        (else (native:magnitude x))
  ) ;cond
) ;define

(define (bn:angle x)
  (cond ((or (bigint? x) (bigrat? x) (bigcomplex? x)) (native:angle (bn:to-inexact x)))
        (else (native:angle x))
  ) ;cond
) ;define

(define (bn:finite? x)
  (or (bigint? x) (bigrat? x) (bigcomplex? x) (native:finite? x))
) ;define

(define (bn:infinite? x)
  (and (not (bigint? x))
    (not (bigrat? x))
    (not (bigcomplex? x))
    (native:infinite? x)
  ) ;and
) ;define

(define (bn:nan? x)
  (and (not (bigint? x)) (not (bigrat? x)) (not (bigcomplex? x)) (native:nan? x))
) ;define
