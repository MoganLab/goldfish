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

(define-library (liii json schema embedded)
  (import (scheme base)
    (liii json)
    (liii json schema data)
    (liii json schema dialect)
    (only (liii uri) uri-join uri->string string->uri)
    (liii json schema pointer)
  ) ;import
  (export resolve-resource-name absolute-resource embedded-resource
    anchor-resource dynamic-anchor-resource uri-suffix? walk-dynamic-anchor
  ) ;export
  (begin
    (define (uri-suffix? uri suffix)
      (let ((uri-length (string-length uri)) (suffix-length (string-length suffix)))
        (and (>= uri-length suffix-length)
          (string=? (substring uri (- uri-length suffix-length) uri-length) suffix)
        ) ;and
      ) ;let
    ) ;define
    (define (join-resource-names base ref)
      (uri->string (uri-join (string->uri base) (string->uri ref)))
    ) ;define
    (define (resolve-resource-name ref root)
      (if (absolute-uri? ref)
        ref
        (let ((id (and (json-object? root) (object-ref root "$id" #f))))
          (if (not (string? id))
            ref
            (if (string=? ref "") id (join-resource-names id ref))
          ) ;if
        ) ;let
      ) ;if
    ) ;define
    (define (absolute-resource schema base)
      (let* ((id (object-ref schema "$id" #f))
             (base-id (and (json-object? base) (object-ref base "$id" #f)))
             (absolute (and (string? id) (resolve-resource-name id base)))
            ) ;
        (cond ((and (equal? schema base) (string? base-id) (absolute-uri? base-id)) schema)
              ((and (string? id)
                 (string? base-id)
                 (absolute-uri? base-id)
                 (uri-suffix? base-id id)
               ) ;and
               base
              ) ;
              ((and absolute (not (absolute-uri? id)))
               (map
                 (lambda (cell)
                   (if (string=? (key->string (car cell)) "$id") (cons (car cell) absolute) cell)
                 ) ;lambda
                 (entries schema)
               ) ;map
              ) ;
              (else schema)
        ) ;cond
      ) ;let*
    ) ;define
    (define (embedded-children entries base uri)
      (let loop
        ((rest entries))
        (if (null? rest)
          #f
          (let ((found (walk-embedded-entry (key->string (caar rest)) (cdar rest) base uri)))
            (if found found (loop (cdr rest)))
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    ;; Only subschema locations are followed. A map keyword's entries map
    ;; names to schemas, so each value is walked directly instead of the
    ;; map object itself.
    (define (walk-embedded-entry keyword value base uri)
      (and (subschema-location? keyword)
        (if (and (schema-map-keyword? keyword) (json-object? value))
          (let loop
            ((rest (entries value)))
            (if (null? rest)
              #f
              (let ((found (walk-embedded (cdar rest) base uri)))
                (if found found (loop (cdr rest)))
              ) ;let
            ) ;if
          ) ;let
          (walk-embedded value base uri)
        ) ;if
      ) ;and
    ) ;define
    (define (walk-embedded schema base uri)
      (cond
       ((json-object? schema)
        (let* ((id (object-ref schema "$id" #f))
               (absolute (and (string? id) (resolve-resource-name id base)))
               (current (absolute-resource schema base))
              ) ;
          (if (and absolute (string=? absolute uri))
            current
            (embedded-children (entries current) (if (string? id) current base) uri)
          ) ;if
        ) ;let*
       ) ;
       ((vector? schema)
        (let loop
          ((index 0))
          (if (= index (vector-length schema))
            #f
            (let ((found (walk-embedded (vector-ref schema index) base uri)))
              (if found found (loop (+ index 1)))
            ) ;let
          ) ;if
        ) ;let
       ) ;
       (else #f)
      ) ;cond
    ) ;define
    (define (embedded-resource root uri)
      (walk-embedded root root uri)
    ) ;define
    (define (anchor-children entries anchor)
      (let loop
        ((rest entries))
        (if (null? rest)
          #f
          (let ((found (walk-anchor-entry (key->string (caar rest)) (cdar rest) anchor)))
            (if found found (loop (cdr rest)))
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (walk-anchor-entry keyword value anchor)
      (and (subschema-location? keyword)
        (if (and (schema-map-keyword? keyword) (json-object? value))
          (let loop
            ((rest (entries value)))
            (if (null? rest)
              #f
              (let ((found (walk-anchor (cdar rest) anchor #f)))
                (if found found (loop (cdr rest)))
              ) ;let
            ) ;if
          ) ;let
          (walk-anchor value anchor #f)
        ) ;if
      ) ;and
    ) ;define
    (define (walk-anchor schema anchor top?)
      (cond
       ((json-object? schema)
        (if (and (not top?) (object-has? schema "$id"))
          #f
          (let ((value (object-ref schema "$anchor" #f)))
            (if (and (string? value) (string=? value anchor))
              schema
              (anchor-children (entries schema) anchor)
            ) ;if
          ) ;let
        ) ;if
       ) ;
       ((vector? schema)
        (let loop
          ((index 0))
          (if (= index (vector-length schema))
            #f
            (let ((found (walk-anchor (vector-ref schema index) anchor #f)))
              (if found found (loop (+ index 1)))
            ) ;let
          ) ;if
        ) ;let
       ) ;
       (else #f)
      ) ;cond
    ) ;define
    (define (anchor-resource root anchor)
      (or (walk-anchor root anchor #t)
        (walk-dynamic-anchor root anchor #t)
        (error 'json-schema-unresolvable anchor)
      ) ;or
    ) ;define
    (define (dynamic-children entries anchor)
      (let loop
        ((rest entries))
        (if (null? rest)
          #f
          (let ((found (walk-dynamic-entry (key->string (caar rest)) (cdar rest) anchor)))
            (if found found (loop (cdr rest)))
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (walk-dynamic-entry keyword value anchor)
      (and (subschema-location? keyword)
        (if (and (schema-map-keyword? keyword) (json-object? value))
          (let loop
            ((rest (entries value)))
            (if (null? rest)
              #f
              (let ((found (walk-dynamic-anchor (cdar rest) anchor #f)))
                (if found found (loop (cdr rest)))
              ) ;let
            ) ;if
          ) ;let
          (walk-dynamic-anchor value anchor #f)
        ) ;if
      ) ;and
    ) ;define
    (define (walk-dynamic-anchor schema anchor top?)
      (cond
       ((json-object? schema)
        (if (and (not top?) (object-has? schema "$id"))
          #f
          (let ((value (object-ref schema "$dynamicAnchor" #f)))
            (if (and (string? value) (string=? value anchor))
              schema
              (dynamic-children (entries schema) anchor)
            ) ;if
          ) ;let
        ) ;if
       ) ;
       ((vector? schema)
        (let loop
          ((index 0))
          (if (= index (vector-length schema))
            #f
            (let ((found (walk-dynamic-anchor (vector-ref schema index) anchor #f)))
              (if found found (loop (+ index 1)))
            ) ;let
          ) ;if
        ) ;let
       ) ;
       (else #f)
      ) ;cond
    ) ;define
    (define (dynamic-anchor-resource root anchor)
      (or (walk-dynamic-anchor root anchor #t)
        (error 'json-schema-unresolvable anchor)
      ) ;or
    ) ;define
  ) ;begin
) ;define-library
