;;; liii/bignum/complex.scm - <bigcomplex> 数值塔适配（由 liii/bignum.scm include）
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

(define (bn:complex-any? x)
  (or (bigcomplex? x)
    (and (not (bigint? x))
      (not (bigrat? x))
      (native:complex? x)
      (not (native:real? x))
    ) ;and
  ) ;or
) ;define

(define (bn:c-real x)
  (cond ((bigcomplex? x) (bigcomplex-real x))
        ((or (bigint? x) (bigrat? x) (native:real? x)) x)
        (else (native:real-part x))
  ) ;cond
) ;define

(define (bn:c-imag x)
  (cond ((bigcomplex? x) (bigcomplex-imag x))
        ((or (bigint? x) (bigrat? x) (native:real? x)) 0)
        (else (native:imag-part x))
  ) ;cond
) ;define

(define (bn:make-complex r i)
  (if (and (bn:exact-rat-any? i) (bn:zero? i))
    r
    (if (and (bn:exact-rat-any? r) (bn:exact-rat-any? i))
      (make-bigcomplex r i)
      (native:make-rectangular (bn:to-inexact r) (bn:to-inexact i))
    ) ;if
  ) ;if
) ;define

(define (bn:bigcomplex->inexact z)
  (native:make-rectangular (bn:to-inexact (bigcomplex-real z))
    (bn:to-inexact (bigcomplex-imag z))
  ) ;native:make-rectangular
) ;define

(define (bn:complex-negate z)
  (bn:make-complex (bn:negate-1 (bn:c-real z)) (bn:negate-1 (bn:c-imag z)))
) ;define

(define (bn:complex-add a b)
  (bn:make-complex (bn:add-2 (bn:c-real a) (bn:c-real b))
    (bn:add-2 (bn:c-imag a) (bn:c-imag b))
  ) ;bn:make-complex
) ;define

(define (bn:complex-sub a b)
  (bn:make-complex (bn:sub-2 (bn:c-real a) (bn:c-real b))
    (bn:sub-2 (bn:c-imag a) (bn:c-imag b))
  ) ;bn:make-complex
) ;define

(define (bn:complex-mul a b)
  (let ((ar (bn:c-real a)) (ai (bn:c-imag a)) (br (bn:c-real b)) (bi (bn:c-imag b)))
    (bn:make-complex (bn:sub-2 (bn:mul-2 ar br) (bn:mul-2 ai bi))
      (bn:add-2 (bn:mul-2 ar bi) (bn:mul-2 ai br))
    ) ;bn:make-complex
  ) ;let
) ;define

(define (bn:complex-div a b)
  (let* ((ar (bn:c-real a))
         (ai (bn:c-imag a))
         (br (bn:c-real b))
         (bi (bn:c-imag b))
         (den (bn:add-2 (bn:mul-2 br br) (bn:mul-2 bi bi)))
        ) ;
    (bn:make-complex (bn:div-2 (bn:add-2 (bn:mul-2 ar br) (bn:mul-2 ai bi)) den)
      (bn:div-2 (bn:sub-2 (bn:mul-2 ai br) (bn:mul-2 ar bi)) den)
    ) ;bn:make-complex
  ) ;let*
) ;define

(define (bn:complex-eq? a b)
  (and (bn:num-eq-2 (bn:c-real a) (bn:c-real b))
    (bn:num-eq-2 (bn:c-imag a) (bn:c-imag b))
  ) ;and
) ;define
