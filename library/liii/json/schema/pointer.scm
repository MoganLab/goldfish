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

(define-library (liii json schema pointer)
  (import (scheme base) (liii json) (liii json schema data) (liii unicode))
  (export resolve-pointer uri-fragment uri-without-fragment percent-decode
    absolute-uri? uri-directory uri-suffix?
  ) ;export
  (begin
    (define (json-pointer-token token)
      (let ((port (open-output-string)))
        (let loop
          ((chars (string->list token)))
          (if (null? chars)
            (get-output-string port)
            (if (and (char=? (car chars) #\~) (pair? (cdr chars)))
              (begin
                (cond ((char=? (cadr chars) #\0) (display "~" port))
                      ((char=? (cadr chars) #\1) (display "/" port))
                      (else (display "~" port) (write-char (cadr chars) port))
                ) ;cond
                (loop (cddr chars))
              ) ;begin
              (begin
                (write-char (car chars) port)
                (loop (cdr chars))
              ) ;begin
            ) ;if
          ) ;if
        ) ;let
      ) ;let
    ) ;define
    (define (split-pointer pointer)
      (let loop
        ((chars (string->list pointer)) (current '()) (result '()))
        (cond
         ((null? chars) (reverse (cons (list->string (reverse current)) result)))
         ((char=? (car chars) #\/)
          (loop (cdr chars) '() (cons (list->string (reverse current)) result))
         ) ;
         (else (loop (cdr chars) (cons (car chars) current) result))
        ) ;cond
      ) ;let
    ) ;define
    (define (pointer-array-index token length)
      (let ((valid?
              (and (> (string-length token) 0)
                (or (string=? token "0")
                  (and (char>=? (string-ref token 0) #\1)
                    (char<=? (string-ref token 0) #\9)
                    (let loop
                      ((index 0))
                      (or (= index (string-length token))
                        (and (char>=? (string-ref token index) #\0)
                          (char<=? (string-ref token index) #\9)
                          (loop (+ index 1))
                        ) ;and
                      ) ;or
                    ) ;let
                  ) ;and
                ) ;or
              ) ;and
            ) ;valid?
           ) ;
        (if (not valid?)
          (error 'json-schema-unresolvable token)
          (let ((index (string->number token)))
            (if (< index length) index (error 'json-schema-unresolvable token))
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (resolve-pointer root pointer)
      (if (string=? pointer "")
        root
        (if (not (char=? (string-ref pointer 0) #\/))
          (error 'json-schema-unresolvable pointer)
          (let loop
            ((value root) (tokens (split-pointer (substring pointer 1))))
            (if (null? tokens)
              value
              (let ((token (json-pointer-token (car tokens))))
                (cond
                 ((json-object? value)
                  (let ((cell (object-cell value token)))
                    (if cell
                      (loop (cdr cell) (cdr tokens))
                      (error 'json-schema-unresolvable pointer)
                    ) ;if
                  ) ;let
                 ) ;
                 ((vector? value)
                  (loop (vector-ref value (pointer-array-index token (vector-length value)))
                    (cdr tokens)
                  ) ;loop
                 ) ;
                 (else (error 'json-schema-unresolvable pointer))
                ) ;cond
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;if
    ) ;define
    (define (uri-fragment uri)
      (let loop
        ((index 0))
        (cond ((= index (string-length uri)) #f)
              ((char=? (string-ref uri index) #\#)
               (substring uri (+ index 1) (string-length uri))
              ) ;
              (else (loop (+ index 1)))
        ) ;cond
      ) ;let
    ) ;define
    (define (uri-without-fragment uri)
      (let loop
        ((index 0))
        (cond ((= index (string-length uri)) uri)
              ((char=? (string-ref uri index) #\#) (substring uri 0 index))
              (else (loop (+ index 1)))
        ) ;cond
      ) ;let
    ) ;define
    (define (hex-value char)
      (cond ((and (char>=? char #\0) (char<=? char #\9)) (- (char->integer char) 48))
            ((and (char>=? char #\a) (char<=? char #\f)) (+ 10 (- (char->integer char) 97)))
            ((and (char>=? char #\A) (char<=? char #\F)) (+ 10 (- (char->integer char) 65)))
            (else #f)
      ) ;cond
    ) ;define
    (define (percent-decode value)
      (let loop
        ((index 0) (bytes '()))
        (if (= index (string-length value))
          (utf8->string (apply bytevector (reverse bytes)))
          (if (char=? (string-ref value index) #\%)
            (if (> (+ index 2) (- (string-length value) 1))
              (error 'json-schema-unresolvable "Incomplete percent escape" value)
              (let ((high (hex-value (string-ref value (+ index 1))))
                    (low (hex-value (string-ref value (+ index 2))))
                   ) ;
                (if (and high low)
                  (loop (+ index 3) (cons (+ (* high 16) low) bytes))
                  (error 'json-schema-unresolvable "Invalid percent escape" value)
                ) ;if
              ) ;let
            ) ;if
            (loop (+ index 1) (cons (char->integer (string-ref value index)) bytes))
          ) ;if
        ) ;if
      ) ;let
    ) ;define
    (define (absolute-uri? value)
      (let loop
        ((index 0))
        (and (< index (string-length value))
          (or (char=? (string-ref value index) #\:) (loop (+ index 1)))
        ) ;and
      ) ;let
    ) ;define
    (define (uri-directory uri)
      (let ((clean (uri-without-fragment uri)))
        (let loop
          ((index (- (string-length clean) 1)))
          (cond ((< index 0) clean)
                ((char=? (string-ref clean index) #\/) (substring clean 0 (+ index 1)))
                (else (loop (- index 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (uri-suffix? uri suffix)
      (let ((uri-length (string-length uri)) (suffix-length (string-length suffix)))
        (and (>= uri-length suffix-length)
          (string=? (substring uri (- uri-length suffix-length) uri-length) suffix)
        ) ;and
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
