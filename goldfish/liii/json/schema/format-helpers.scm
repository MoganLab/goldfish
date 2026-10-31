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

(define-library (liii json schema format-helpers)
  (import (scheme base) (scheme char) (liii string))
  (export json-schema-ascii-digit? json-schema-ascii-letter?
    json-schema-ascii-alnum? json-schema-hex-digit? json-schema-all-ascii?
    json-schema-split-on-char json-schema-decimal-string?
    json-schema-decimal->number json-schema-hex-string?
  ) ;export
  (begin
    ;; ---- shared helpers -------------------------------------------------

    (define (json-schema-ascii-digit? char)
      (and (char>=? char #\0) (char<=? char #\9))
    ) ;define
    (define (json-schema-ascii-letter? char)
      (or (and (char>=? char #\a) (char<=? char #\z))
        (and (char>=? char #\A) (char<=? char #\Z))
      ) ;or
    ) ;define
    (define (json-schema-ascii-alnum? char)
      (or (json-schema-ascii-digit? char) (json-schema-ascii-letter? char))
    ) ;define
    (define (json-schema-hex-digit? char)
      (or (json-schema-ascii-digit? char)
        (and (char>=? char #\a) (char<=? char #\f))
        (and (char>=? char #\A) (char<=? char #\F))
      ) ;or
    ) ;define
    (define (json-schema-all-ascii? value)
      (let loop
        ((index 0))
        (cond ((= index (string-length value)) #t)
              ((> (char->integer (string-ref value index)) 127) #f)
              (else (loop (+ index 1)))
        ) ;cond
      ) ;let
    ) ;define
    (define (json-schema-split-on-char value separator)
      (let loop
        ((index 0) (current '()) (result '()))
        (cond
         ((= index (string-length value))
          (reverse (cons (list->string (reverse current)) result))
         ) ;
         ((char=? (string-ref value index) separator)
          (loop (+ index 1) '() (cons (list->string (reverse current)) result))
         ) ;
         (else (loop (+ index 1) (cons (string-ref value index) current) result))
        ) ;cond
      ) ;let
    ) ;define
    (define (json-schema-decimal-string? value)
      (and (> (string-length value) 0)
        (let loop
          ((index 0))
          (cond ((= index (string-length value)) #t)
                ((json-schema-ascii-digit? (string-ref value index)) (loop (+ index 1)))
                (else #f)
          ) ;cond
        ) ;let
      ) ;and
    ) ;define
    (define (json-schema-decimal->number value)
      (let loop
        ((index 0) (result 0))
        (if (= index (string-length value))
          result
          (loop (+ index 1)
            (+ (* result 10) (- (char->integer (string-ref value index)) 48))
          ) ;loop
        ) ;if
      ) ;let
    ) ;define
    (define (json-schema-hex-string? value)
      (and (> (string-length value) 0)
        (<= (string-length value) 4)
        (let loop
          ((index 0))
          (cond ((= index (string-length value)) #t)
                ((json-schema-hex-digit? (string-ref value index)) (loop (+ index 1)))
                (else #f)
          ) ;cond
        ) ;let
      ) ;and
    ) ;define
  ) ;begin
) ;define-library
