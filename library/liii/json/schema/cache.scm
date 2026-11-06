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

(define-library (liii json schema cache)
  (import (scheme base))
  (export make-reference-cache reference-cache-key-matches?
    reference-cache-lookup reference-cache-put!
  ) ;export
  (begin
    ;; A cache resolves a reference by looking it up under a key that
    ;; describes everything the resolution depends on: the reference
    ;; string, the root resource it was resolved against and the dynamic
    ;; scope that was active.
    ;;
    ;; The cache is a single element vector holding the entry list, so
    ;; entries can be added without rebinding the cache itself.
    ;;
    ;; An entry is a flat four element list
    ;;   (reference root scope value)
    ;; so that reading a field never needs a nested car or cdr.

    (define (make-reference-cache)
      (vector '())
    ) ;define

    ;; The root and every scope entry are compared by identity, so two
    ;; different resources never share a cache entry.
    (define (scope-matches? left right)
      (let loop
        ((xs left) (ys right))
        (cond ((and (null? xs) (null? ys)) #t)
              ((or (null? xs) (null? ys)) #f)
              ((eq? (car xs) (car ys)) (loop (cdr xs) (cdr ys)))
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    ;; A malformed entry never breaks validation: the lookup simply
    ;; misses and the reference is resolved again.
    (define (reference-cache-key-matches? entry reference root scope)
      (guard (condition (#t #f))
        (and (string=? (car entry) reference)
          (eq? (cadr entry) root)
          (scope-matches? (caddr entry) scope)
        ) ;and
      ) ;guard
    ) ;define

    ;; Returns the cached value or #f when the entry is unknown.
    ;; The value may itself be #f, so the caller has to distinguish a
    ;; miss from a cached false schema by using the entry directly.
    (define (reference-cache-lookup cache reference root scope)
      (let loop
        ((entries (vector-ref cache 0)))
        (cond ((null? entries) #f)
              ((reference-cache-key-matches? (car entries) reference root scope)
               (car entries)
              ) ;
              (else (loop (cdr entries)))
        ) ;cond
      ) ;let
    ) ;define

    (define (reference-cache-put! cache reference root scope value)
      (vector-set! cache
        0
        (cons (list reference root scope value) (vector-ref cache 0))
      ) ;vector-set!
      value
    ) ;define
  ) ;begin
) ;define-library
