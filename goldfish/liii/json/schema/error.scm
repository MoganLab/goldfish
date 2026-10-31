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

(define-library (liii json schema error)
  (import (scheme base) (scheme write) (scheme char) (liii json schema data))
  (export json-schema-error? json-schema-error-kind json-schema-error-message
    json-schema-error-keyword json-schema-error-keyword-value
    json-schema-error-instance json-schema-error-schema json-schema-error-path
    json-schema-error-schema-path json-schema-error-context
    json-schema-error-cause json-schema-error-absolute-path
    json-schema-error-absolute-schema-path json-schema-error-json-path
    make-validation-error validation-error value->display
  ) ;export
  (begin
    (define-record-type <json-schema-error>
      (make-validation-error kind message keyword keyword-value instance schema
        path schema-path context cause
      ) ;make-validation-error
      json-schema-error?
      (kind json-schema-error-kind)
      (message json-schema-error-message)
      (keyword json-schema-error-keyword)
      (keyword-value json-schema-error-keyword-value)
      (instance json-schema-error-instance)
      (schema json-schema-error-schema)
      (path json-schema-error-path)
      (schema-path json-schema-error-schema-path)
      (context json-schema-error-context)
      (cause json-schema-error-cause)
    ) ;define-record-type
    (define (json-schema-error-absolute-path err)
      (json-schema-error-path err)
    ) ;define
    (define (json-schema-error-absolute-schema-path err)
      (json-schema-error-schema-path err)
    ) ;define
    (define (value->display value)
      (let ((port (open-output-string)))
        (write value port)
        (get-output-string port)
      ) ;let
    ) ;define
    (define (validation-error message keyword keyword-value instance schema path
              schema-path context cause
            ) ;validation-error
      (make-validation-error 'validation
        message
        keyword
        keyword-value
        instance
        schema
        path
        (if keyword (append schema-path (list keyword)) schema-path)
        (or context '())
        cause
      ) ;make-validation-error
    ) ;define
    (define (identifier-start? char)
      (or (char-alphabetic? char) (char=? char #\_) (char=? char #\$))
    ) ;define
    (define (identifier-part? char)
      (or (identifier-start? char) (char-numeric? char))
    ) ;define
    (define (json-identifier? value)
      (and (> (string-length value) 0)
        (identifier-start? (string-ref value 0))
        (let loop
          ((index 1))
          (or (= index (string-length value))
            (and (identifier-part? (string-ref value index)) (loop (+ index 1)))
          ) ;or
        ) ;let
      ) ;and
    ) ;define
    (define (write-path-key port value)
      (for-each
        (lambda (char)
          (cond ((or (char=? char #\\) (char=? char #\'))
                 (write-char #\\ port)
                 (write-char char port)
                ) ;
                ((char=? char #\newline) (display "\\n" port))
                ((char=? char #\return) (display "\\r" port))
                ((char=? char #\tab) (display "\\t" port))
                (else (write-char char port))
          ) ;cond
        ) ;lambda
        (string->list value)
      ) ;for-each
    ) ;define
    (define (json-schema-error-json-path err)
      (let ((port (open-output-string)))
        (display "$" port)
        (for-each
          (lambda (part)
            (cond ((integer? part) (display "[" port) (display part port) (display "]" port))
                  ((json-identifier? (key->string part))
                   (display "." port)
                   (display (key->string part) port)
                  ) ;
                  (else (display "['" port)
                    (write-path-key port (key->string part))
                    (display "']" port)
                  ) ;else
            ) ;cond
          ) ;lambda
          (json-schema-error-path err)
        ) ;for-each
        (get-output-string port)
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
