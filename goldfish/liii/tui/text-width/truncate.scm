;;
;; Copyright (C) 2024 The Goldfish Scheme Authors
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

(define-library (liii tui text-width truncate)
  (export truncate-to-width)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii unicode)
    (liii tui text-width core)
  ) ;import
  (begin

    (define (%sub-ellipsis ellipsis width)
      (let loop
        ((chars (string->list ellipsis)) (acc '()) (cw 0))
        (if (or (null? chars) (>= cw width))
          (list->string (reverse acc))
          (loop (cdr chars) (cons (car chars) acc) (+ cw 1))
        ) ;if
      ) ;let
    ) ;define

    (define (%finalize-truncated acc ellipsis had-ansi?)
      (let ((base (apply string-append (reverse (cons ellipsis acc)))))
        (if had-ansi? (string-append base "\x1b;[0m") base)
      ) ;let
    ) ;define

    (define (%truncate-loop bv len target-w ellipsis)
      (let loop
        ((i 0) (cur-w 0) (acc '()) (had-ansi? #f))
        (cond ((>= i len) (%finalize-truncated acc ellipsis had-ansi?))
              ((%match-ansi-csi bv i len)
               =>
               (lambda (ansi-end)
                 (let ((chunk (utf8->string (bytevector-copy bv i ansi-end))))
                   (loop ansi-end cur-w (cons chunk acc) #t)
                 ) ;let
               ) ;lambda
              ) ;
              (else
                (let* ((next (bytevector-advance-utf8 bv i len))
                       (step (if (= next i) (+ i 1) next))
                       (cp (utf8->codepoint-at bv i))
                       (cw (%codepoint-width cp))
                      ) ;
                  (if (> (+ cur-w cw) target-w)
                    (%finalize-truncated acc ellipsis had-ansi?)
                    (let ((chunk (utf8->string (bytevector-copy bv i step))))
                      (loop step (+ cur-w cw) (cons chunk acc) had-ansi?)
                    ) ;let
                  ) ;if
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (truncate-to-width str width . args)
      (unless (string? str)
        (type-error "truncate-to-width: str must be a string")
      ) ;unless
      (unless (real? width)
        (type-error "truncate-to-width: width must be a number")
      ) ;unless
      (let ((ellipsis (if (null? args) "..." (car args))))
        (unless (string? ellipsis)
          (type-error "truncate-to-width: ellipsis must be a string")
        ) ;unless
        (let ((w (inexact->exact (floor width))))
          (cond ((<= w 0) "")
                ((string=? str "") "")
                ((<= (display-width str) w) str)
                (else
                  (let ((target-w (- w (display-width ellipsis))))
                    (if (< target-w 0)
                      (%sub-ellipsis ellipsis w)
                      (let ((bv (string->utf8 str)))
                        (%truncate-loop bv (bytevector-length bv) target-w ellipsis)
                      ) ;let
                    ) ;if
                  ) ;let
                ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
