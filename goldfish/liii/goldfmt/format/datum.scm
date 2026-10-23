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

(define-library (liii goldfmt format datum)
  (export single-arg-symbol-form? quote-syntax-form? raw-string-datum?
    newline-marker-datum? comment-datum? reader-newlines
    reader-datum-contains-newline-marker? reader-head-name reader-rule-head?
    reader-form-like? reader-selected-item reader-selected-column
    reader-selected-text reader-skip-selected reader-first-selected-form-column
    reader-by-first-rest-child-indent reader-rest-indent reader-append-selected
    last-line-column reader-fill-atom? reader-fill-eligible? reader-append-close
    reader-vector-prefix reader-vector->list format-reader-pair-fill
    reader-keyword-block-info
  ) ;export
  (import (liii base)
    (liii goldfmt record)
    (liii goldfmt rule)
    (liii goldfmt format atom)
  ) ;import
  (begin
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

    (define (reader-vector-prefix datum)
      (if (byte-vector? datum) "#u8(" "#(")
    ) ;define

    (define (reader-vector->list datum)
      (let loop
        ((i 0) (result '()))
        (if (>= i (vector-length datum))
          (reverse result)
          (loop (+ i 1) (cons (vector-ref datum i) result))
        ) ;if
      ) ;let
    ) ;define

    (define (format-reader-pair-fill datum indent fmt-inline)
      (let* ((head (car datum))
             (rule-head? (reader-rule-head? head))
             (rest-indent (+ indent (if rule-head? 2 1)))
            ) ;
        (let loop
          ((current datum) (column (+ indent 1)) (first? #t) (pieces (list "(")))
          (if (null? current)
            (apply string-append (reverse (cons ")" pieces)))
            (let* ((item (car current))
                   (text (fmt-inline item))
                   (separator (if first? "" " "))
                   (needed (+ (string-length separator) (string-length text)))
                  ) ;
              (if (and (not first?) (> (+ column needed) max-inline-length))
                (loop (cdr current)
                  (+ rest-indent (string-length text))
                  #f
                  (cons text (cons (string-append "\n" (spaces rest-indent)) pieces))
                ) ;loop
                (loop (cdr current) (+ column needed) #f (cons text (cons separator pieces)))
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (reader-keyword-block-info current rest-indent fmt-inline)
      (let collect
        ((curr current) (pairs '()))
        (define (finish)
          (keyword-block-prefix (reverse pairs) rest-indent)
        ) ;define
        (if
          (or (not (pair? curr)) (not (pair? (cdr curr))))
          (finish)
          (let ((key (car curr)) (val (cadr curr)))
            (if (and (keyword? key)
                  (not (newline-marker-datum? val))
                  (not (comment-datum? val))
                ) ;and
              (let ((val-inline (fmt-inline val)))
                (if (not (string-contains-newline? val-inline))
                  (let ((key-len (string-length (fmt-inline key)))
                        (val-len (string-length val-inline))
                       ) ;
                    (if (<= (+ rest-indent key-len 1 val-len) max-inline-length)
                      (collect (cddr curr) (cons (cons key-len val-len) pairs))
                      (finish)
                    ) ;if
                  ) ;let
                  (finish)
                ) ;if
              ) ;let
              (finish)
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
