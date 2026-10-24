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
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(define-library (liii goldfmt format atom)
  (export write-to-string string-contains-newline? format-atom-value spaces
    child-separator comment-node? newline-node? newline-count make-newlines
    comment-content format-comment-content contains-comment? quote-env?
    reader-prefix-env? quote-env-content keyword-node? keyword-node-len
    keyword-block-prefix no-keyword-args-form? fill-min-children let-form?
  ) ;export
  (import (liii base)
    (liii vector)
    (liii goldfmt record)
    (liii goldfmt rule)
    (liii goldfmt scan normalize)
  ) ;import
  (begin
    (define (write-to-string value)
      (let ((port (open-output-string)))
        (let-temporarily (((*s7* 'print-length) 9223372036854775807))
          (write value port)
        ) ;let-temporarily
        (get-output-string port)
      ) ;let
    ) ;define

    (define (string-contains-newline? text)
      (if (string-position "\n" text) #t #f)
    ) ;define

    (define (format-atom-value value)
      (cond ((raw-string-literal? value) (raw-string-literal-source value))
            ((char-literal? value) (char-literal-source value))
            ((symbol? value) (symbol->string value))
            ((number? value) (number->string value))
            (else (write-to-string value))
      ) ;cond
    ) ;define

    (define (spaces n)
      (if (<= n 0) "" (make-string n #\space))
    ) ;define

    (define (child-separator parent child-index)
      (if (and (string=? (env-tag-name parent) "") (= child-index 0)) "" " ")
    ) ;define

    (define (comment-node? node)
      (and (env? node)
        (string=? (env-tag-name node) "*comment*")
        (= (vector-length (env-children node)) 1)
        (let ((child (vector-ref (env-children node) 0)))
          (and (atom? child) (string? (atom-value child)))
        ) ;let
      ) ;and
    ) ;define

    (define (newline-node? node)
      (and (env? node)
        (string=? (env-tag-name node) "*newline*")
        (= (vector-length (env-children node)) 1)
        (let ((child (vector-ref (env-children node) 0)))
          (and (atom? child) (number? (atom-value child)))
        ) ;let
      ) ;and
    ) ;define

    (define (newline-count node)
      (atom-value (vector-ref (env-children node) 0))
    ) ;define

    (define (make-newlines n)
      (if (<= n 1) "" (make-string (- n 1) #\newline))
    ) ;define

    (define (comment-content node)
      (atom-value (vector-ref (env-children node) 0))
    ) ;define

    (define (format-comment-content content)
      (if (or (string=? content "")
            (char=? (string-ref content 0) #\space)
            (char=? (string-ref content 0) #\;)
          ) ;or
        (string-append ";;" content)
        (string-append ";; " content)
      ) ;if
    ) ;define

    (define (contains-comment? node)
      (cond ((comment-node? node) #t)
            ((atom? node) #f)
            (else (vector-any contains-comment? (env-children node)))
      ) ;cond
    ) ;define

    (define (keyword-node? node)
      (and (atom? node) (keyword? (atom-value node)))
    ) ;define

    (define (keyword-node-len node)
      (string-length (format-atom-value (atom-value node)))
    ) ;define

    (define (keyword-block-prefix pairs indent)
      (let loop
        ((rest pairs) (count 0) (max-key-len 0) (max-val-len 0))
        (cond ((null? rest) (values count max-key-len))
              ((<= (+ indent (max max-key-len (caar rest)) 1 (max max-val-len (cdar rest)))
                 max-inline-length
               ) ;<=
               (loop (cdr rest)
                 (+ count 1)
                 (max max-key-len (caar rest))
                 (max max-val-len (cdar rest))
               ) ;loop
              ) ;
              (else (values count max-key-len))
        ) ;cond
      ) ;let
    ) ;define

    (define (no-keyword-args-form? tag-name)
      (and (member tag-name
             '("define" "define*" "define-values" "define-syntax" "define-macro"
               "define-record-type" "define-library" "import" "export")
           ) ;member
        #t
      ) ;and
    ) ;define

    (define (let-form? tag-name)
      (if (member tag-name '("let" "let*" "letrec" "letrec*" "let-values"
                             "let*-values"))
        #t
        #f
      ) ;if
    ) ;define

    (define (quote-env? node)
      (and (not (stem-mode?))
        (env? node)
        (or (string=? (env-tag-name node) "quote")
          (string=? (env-tag-name node) "#_quote")
        ) ;or
      ) ;and
    ) ;define

    (define (reader-prefix-env? node)
      (and (not (stem-mode?))
        (env? node)
        (or (string=? (env-tag-name node) "quasiquote")
          (string=? (env-tag-name node) "unquote")
          (string=? (env-tag-name node) "unquote-splicing")
        ) ;or
      ) ;and
    ) ;define

    (define (quote-env-content node)
      (let ((value (env-value node)))
        (if
          (and (pair? value) (not (null? (cdr value))))
          (cadr value)
          '()
        ) ;if
      ) ;let
    ) ;define

    (define fill-min-children 4)
  ) ;begin
) ;define-library
