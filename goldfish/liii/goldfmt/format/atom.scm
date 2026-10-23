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
    keyword-block-prefix no-keyword-args-form? single-arg-symbol-form?
    quote-syntax-form? raw-string-datum? newline-marker-datum? comment-datum?
    reader-newlines reader-datum-contains-newline-marker? reader-head-name
    reader-rule-head? reader-form-like? reader-selected-item
    reader-selected-column reader-selected-text reader-skip-selected
    reader-first-selected-form-column reader-by-first-rest-child-indent
    reader-rest-indent reader-append-selected last-line-column reader-fill-atom?
    reader-fill-eligible? reader-append-close fill-min-children let-form?
  ) ;export
  (import (liii base)
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
            (else
              (let ((children (env-children node)))
                (let loop
                  ((i 0))
                  (cond ((>= i (vector-length children)) #f)
                        ((contains-comment? (vector-ref children i)) #t)
                        (else (loop (+ i 1)))
                  ) ;cond
                ) ;let
              ) ;let
            ) ;else
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

    (define (single-arg-symbol-form? value name)
      (and (pair? value)
        (eq? (car value) name)
        (pair? (cdr value))
        (null? (cddr value))
      ) ;and
    ) ;define

    (define (quote-syntax-form? value)
      (and (pair? value)
        (not (null? value))
        (syntax? (car value))
        (string=? (object->string (car value) #f) "#_quote")
        (not (null? (cdr value)))
        (null? (cddr value))
      ) ;and
    ) ;define

    (define (raw-string-datum? datum)
      (and (pair? datum)
        (eq? (car datum) '*raw-string*)
        (pair? (cdr datum))
        (string? (cadr datum))
        (pair? (cddr datum))
        (string? (caddr datum))
        (null? (cdddr datum))
      ) ;and
    ) ;define

    (define (newline-marker-datum? datum)
      (and (pair? datum)
        (eq? (car datum) '*newline*)
        (pair? (cdr datum))
        (number? (cadr datum))
        (null? (cddr datum))
      ) ;and
    ) ;define

    (define (comment-datum? datum)
      (and (pair? datum)
        (eq? (car datum) '*comment*)
        (pair? (cdr datum))
        (string? (cadr datum))
        (null? (cddr datum))
      ) ;and
    ) ;define

    (define (reader-newlines count)
      (let loop
        ((i count) (result ""))
        (if (<= i 0) result (loop (- i 1) (string-append result "\n")))
      ) ;let
    ) ;define

    (define (reader-datum-contains-newline-marker? datum)
      (cond ((newline-marker-datum? datum) #t)
            ((comment-datum? datum) #t)
            ((pair? datum)
             (or (reader-datum-contains-newline-marker? (car datum))
               (reader-datum-contains-newline-marker? (cdr datum))
             ) ;or
            ) ;
            ((or (vector? datum) (byte-vector? datum))
             (let loop
               ((i 0))
               (cond ((>= i (vector-length datum)) #f)
                     ((reader-datum-contains-newline-marker? (vector-ref datum i)) #t)
                     (else (loop (+ i 1)))
               ) ;cond
             ) ;let
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    (define (reader-head-name head)
      (cond ((symbol? head) (symbol->string head))
            ((syntax? head)
             (let ((name (object->string head #f)))
               (if (string=? name "#_quote") "quote" name)
             ) ;let
            ) ;
            (else "default")
      ) ;cond
    ) ;define

    (define (reader-rule-head? head)
      (or (symbol? head) (syntax? head))
    ) ;define

    (define (reader-form-like? datum)
      (pair? datum)
    ) ;define

    (define (reader-selected-item item)
      (car item)
    ) ;define

    (define (reader-selected-column item)
      (cadr item)
    ) ;define

    (define (reader-selected-text item)
      (caddr item)
    ) ;define

    (define (reader-skip-selected rest selected)
      (let loop
        ((current rest) (remaining (length selected)))
        (if (or (<= remaining 0) (not (pair? current)))
          current
          (loop (cdr current) (- remaining 1))
        ) ;if
      ) ;let
    ) ;define

    (define (reader-first-selected-form-column selected)
      (cond ((null? selected) #f)
            ((reader-form-like? (reader-selected-item (car selected)))
             (reader-selected-column (car selected))
            ) ;
            (else (reader-first-selected-form-column (cdr selected)))
      ) ;cond
    ) ;define

    (define (reader-by-first-rest-child-indent parent-indent rest)
      (if
        (and (pair? rest)
          (reader-form-like? (car rest))
          (let ((name (reader-head-name (car (car rest)))))
            (string=? name "")
          ) ;let
        ) ;and
        (+ parent-indent 1)
        (+ parent-indent 2)
      ) ;if
    ) ;define

    (define (reader-rest-indent head-name parent-indent selected rest)
      (let ((strategy (rest-indent head-name)))
        (cond
         ((eq? strategy 'align-to-first-selected-env)
          (let ((column (reader-first-selected-form-column selected)))
            (if column column (reader-by-first-rest-child-indent parent-indent rest))
          ) ;let
         ) ;
         ((eq? strategy 'parent-plus2) (+ parent-indent 2))
         (else (reader-by-first-rest-child-indent parent-indent rest))
        ) ;cond
      ) ;let
    ) ;define

    (define (reader-append-selected result selected)
      (let loop
        ((items selected) (text result))
        (if (null? items)
          text
          (loop (cdr items) (string-append text " " (reader-selected-text (car items))))
        ) ;if
      ) ;let
    ) ;define

    (define (last-line-column text)
      (let loop
        ((i 0) (column 0))
        (if (>= i (string-length text))
          column
          (loop (+ i 1) (if (char=? (string-ref text i) #\newline) 0 (+ column 1)))
        ) ;if
      ) ;let
    ) ;define

    (define fill-min-children 4)

    (define (reader-fill-atom? item)
      (and (not (pair? item)) (not (vector? item)) (not (byte-vector? item)))
    ) ;define

    (define (reader-fill-eligible? datum)
      (let loop
        ((current datum) (count 0))
        (cond ((null? current) (>= count fill-min-children))
              ((and (pair? current) (keyword? (car current))) #f)
              ((and (pair? current) (reader-fill-atom? (car current)))
               (loop (cdr current) (+ count 1))
              ) ;
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (reader-append-close result close-indent)
      (if (string-suffix? ";#" result)
        (string-append result "\n" (spaces close-indent) ")")
        (string-append result ")")
      ) ;if
    ) ;define
  ) ;begin
) ;define-library
