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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii packrat error)
  (import (scheme base) (srfi srfi-1) (liii packrat position))
  (export parse-error? parse-error-position parse-error-expected
    parse-error-messages make-error-expected make-error-message
    parse-error-empty? merge-parse-errors
  ) ;export
  (begin
    (define-record-type <parse-error>
      (make-parse-error position expected messages)
      parse-error?
      (position parse-error-position)
      (expected parse-error-expected)
      (messages parse-error-messages)
    ) ;define-record-type

    (define (make-error-expected pos str)
      (make-parse-error pos (list str) '())
    ) ;define

    (define (make-error-message pos msg)
      (make-parse-error pos '() (list msg))
    ) ;define

    (define (parse-error-empty? e)
      (and (null? (parse-error-expected e)) (null? (parse-error-messages e)))
    ) ;define

    (define (lset-union = . lists)
      (reduce
        (lambda (lis ans)
          (cond ((null? lis) ans)
                ((null? ans) lis)
                ((eq? lis ans) ans)
                (else
                  (fold
                    (lambda (elt ans) (if (any (lambda (x) (= x elt)) ans) ans (cons elt ans)))
                    ans
                    lis
                  ) ;fold
                ) ;else
          ) ;cond
        ) ;lambda
        '()
        lists
      ) ;reduce
    ) ;define

    (define (merge-parse-errors e1 e2)
      (cond ((not e1) e2)
            ((not e2) e1)
            (else
              (let ((p1 (parse-error-position e1)) (p2 (parse-error-position e2)))
                (cond ((or (parse-position>? p1 p2) (parse-error-empty? e2)) e1)
                      ((or (parse-position>? p2 p1) (parse-error-empty? e1)) e2)
                      (else (make-parse-error p1
                              (lset-union equal? (parse-error-expected e1) (parse-error-expected e2))
                              (lset-union equal? (parse-error-messages e1) (parse-error-messages e2))
                            ) ;make-parse-error
                      ) ;else
                ) ;cond
              ) ;let
            ) ;else
      ) ;cond
    ) ;define
  ) ;begin
) ;define-library
