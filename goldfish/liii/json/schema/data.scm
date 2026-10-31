;;
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
;;

(define-library (liii json schema data)
  (import (scheme base) (liii json))
  (export json-boolean? json-true? json-false? key->string entries object-cell
    object-ref object-has? option-ref listify absent collect-for-each
  ) ;export
  (begin
    (define (json-boolean? x)
      (or (boolean? x) (eq? x 'true) (eq? x 'false))
    ) ;define
    (define (json-true? x)
      (or (eq? x #t) (eq? x 'true))
    ) ;define
    (define (json-false? x)
      (or (eq? x #f) (eq? x 'false))
    ) ;define
    (define (key->string key)
      (cond ((string? key) key)
            ((symbol? key) (symbol->string key))
            (else (error 'type-error "JSON object key must be a string or symbol" key))
      ) ;cond
    ) ;define
    (define (entries object)
      (if (equal? object '(())) '() object)
    ) ;define
    (define (object-cell object key)
      (let ((wanted (key->string key)))
        (let loop
          ((rest (entries object)))
          (cond ((null? rest) #f)
                ((string=? (key->string (caar rest)) wanted) (car rest))
                (else (loop (cdr rest)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (object-ref object key default)
      (let ((cell (object-cell object key)))
        (if cell (cdr cell) default)
      ) ;let
    ) ;define
    (define (object-has? object key)
      (and (object-cell object key) #t)
    ) ;define
    (define absent (list 'absent))
    (define (option-ref options key default)
      (let ((cell (assoc key options)))
        (if cell (cdr cell) default)
      ) ;let
    ) ;define
    (define (listify value)
      (cond ((vector? value) (vector->list value))
            ((null? value) '())
            ((pair? value) value)
            (else (list value))
      ) ;cond
    ) ;define
    (define (collect-for-each proc values)
      (let loop
        ((rest values) (result '()))
        (if (null? rest)
          (reverse result)
          (let ((collected (proc (car rest))))
            (loop (cdr rest) (append (reverse collected) result))
          ) ;let
        ) ;if
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
