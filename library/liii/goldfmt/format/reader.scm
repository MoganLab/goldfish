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

(define-library (liii goldfmt format reader)
  (export format-reader-datum format-reader-datum-at format-reader-datum-inline)
  (import (liii base)
    (liii string)
    (liii goldfmt record)
    (liii goldfmt rule)
    (liii goldfmt format atom)
    (liii goldfmt format datum)
    (liii goldfmt scan normalize)
    (srfi srfi-13)
  ) ;import
  (begin
    (define (reader-select-first-line-items head-name rest start-column)
      (let ((limit (first-line-limit head-name))
            (allow-child-env? (allow-first-line-child-env? head-name))
           ) ;
        (let loop
          ((current rest)
           (column start-column)
           (selected-count 0)
           (direct-env-count 0)
           (result '())
          ) ;
          (if (or (not (pair? current))
                (>= selected-count limit)
                (newline-marker-datum? (car current))
                (keyword? (car current))
              ) ;or
            (reverse result)
            (let* ((item (car current))
                   (item-is-env? (reader-form-like? item))
                   (next-direct-env-count (if item-is-env? (+ direct-env-count 1) direct-env-count)
                   ) ;next-direct-env-count
                  ) ;
              (if (or (and item-is-env? (not allow-child-env?)) (> next-direct-env-count 1))
                (reverse result)
                (let* ((item-column (+ column 1))
                       (item-text (format-reader-datum-at item item-column))
                       (next-result (cons (list item item-column item-text) result))
                       (next-column (+ item-column (string-length item-text)))
                      ) ;
                  (if (string-contains? item-text "\n")
                    (reverse next-result)
                    (loop (cdr current)
                      next-column
                      (+ selected-count 1)
                      next-direct-env-count
                      next-result
                    ) ;loop
                  ) ;if
                ) ;let*
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (format-reader-vector-inline datum)
      (string-append (if (byte-vector? datum) "#u8(" "#(")
        (string-join (map format-reader-datum-inline (vector->list datum)) " ")
        ")"
      ) ;string-append
    ) ;define

    (define (format-reader-vector-multiline datum indent)
      (let* ((prefix (reader-vector-prefix datum))
             (item-indent (+ indent (string-length prefix)))
             (close-marker (string-append "\n" (spaces indent) ") ;#"))
            ) ;
        (let loop
          ((items (vector->list datum)) (pieces (list prefix)) (prefix-ready? #t))
          (if (null? items)
            (apply string-append (reverse (cons close-marker pieces)))
            (let ((item (car items)))
              (if (newline-marker-datum? item)
                (loop (cdr items)
                  (cons (spaces item-indent) (cons (reader-newlines (cadr item)) pieces))
                  #t
                ) ;loop
                (let ((item-text (string-trim (format-reader-datum-at item item-indent))))
                  (loop (cdr items)
                    (cons item-text
                      (if prefix-ready?
                        pieces
                        (cons (string-append "\n" (spaces item-indent)) pieces)
                      ) ;if
                    ) ;cons
                    #f
                  ) ;loop
                ) ;let
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (format-reader-vector-at datum indent)
      (let ((candidate (format-reader-vector-inline datum)))
        (if
          (or (= (vector-length datum) 0)
            (and (not (reader-datum-contains-newline-marker? datum))
              (not (string-contains? candidate "\n"))
              (<= (+ indent (string-length candidate)) max-inline-length)
            ) ;and
          ) ;or
          candidate
          (format-reader-vector-multiline datum indent)
        ) ;if
      ) ;let
    ) ;define

    (define (format-reader-pair-inline datum)
      (call-with-values (lambda () (split-pair-list datum))
        (lambda (elems tail)
          (string-append "("
            (string-join (map format-reader-datum-inline elems) " ")
            (if (null? tail) "" (string-append " . " (format-reader-datum-inline tail)))
            ")"
          ) ;string-append
        ) ;lambda
      ) ;call-with-values
    ) ;define

    (define (reader-append-keyword-block current result rest-indent
              prefix-ready? close-indent block-count max-key-len
            ) ;reader-append-keyword-block
      (let emit-block
        ((curr current) (k 0) (res result) (ready? prefix-ready?))
        (if (>= k block-count)
          (reader-append-rest curr res rest-indent #f close-indent)
          (let* ((key-text (format-reader-datum-inline (car curr)))
                 (val-inline (format-reader-datum-inline (cadr curr)))
                 (key-line-prefix (if ready? "" (string-append "\n" (spaces rest-indent))))
                 (sep-spaces
                   (spaces (+ (- max-key-len (string-length key-text)) 1))
                 ) ;sep-spaces
                ) ;
            (emit-block (cddr curr)
              (+ k 1)
              (string-append res key-line-prefix key-text sep-spaces val-inline)
              #f
            ) ;emit-block
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (reader-append-keyword-single current result rest-indent
              prefix-ready? close-indent
            ) ;reader-append-keyword-single
      (let* ((key-text (format-reader-datum-inline (car current)))
             (key-len (string-length key-text))
             (val-item (cadr current))
             (key-line-prefix (if prefix-ready? "" (string-append "\n" (spaces rest-indent)))
             ) ;key-line-prefix
             (val-inline (format-reader-datum-inline val-item))
             (fits-inline?
               (and (not (string-contains? val-inline "\n"))
                 (<= (+ rest-indent key-len 1 (string-length val-inline)) max-inline-length)
               ) ;and
             ) ;fits-inline?
            ) ;
        (if fits-inline?
          (reader-append-rest (cddr current)
            (string-append result key-line-prefix key-text " " val-inline)
            rest-indent
            #f
            close-indent
          ) ;reader-append-rest
          (reader-append-rest (cddr current)
            (string-append result
              key-line-prefix
              key-text
              "\n"
              (spaces rest-indent)
              (format-reader-datum-at val-item rest-indent)
            ) ;string-append
            rest-indent
            #f
            close-indent
          ) ;reader-append-rest
        ) ;if
      ) ;let*
    ) ;define

    (define (reader-append-normal-item current result rest-indent prefix-ready?
              close-indent
            ) ;reader-append-normal-item
      (let* ((item (car current))
             (is-last-comment? (and (null? (cdr current)) (comment-datum? item)))
             (item-text (format-reader-datum-at item
                          (if prefix-ready? (last-line-column result) rest-indent)
                        ) ;format-reader-datum-at
             ) ;item-text
            ) ;
        (reader-append-rest (cdr current)
          (string-append result
            (if prefix-ready? "" (string-append "\n" (spaces rest-indent)))
            item-text
            (if is-last-comment? (string-append "\n" (spaces close-indent)) "")
          ) ;string-append
          rest-indent
          #f
          close-indent
        ) ;reader-append-rest
      ) ;let*
    ) ;define

    (define (reader-append-rest current result rest-indent prefix-ready? close-indent)
      (cond
       ((pair? current)
        (let ((item (car current)))
          (cond
           ((newline-marker-datum? item)
            (reader-append-rest (cdr current)
              (string-append result (reader-newlines (cadr item)) (spaces rest-indent))
              rest-indent
              #t
              close-indent
            ) ;reader-append-rest
           ) ;
           ((and (keyword? item)
              (pair? (cdr current))
              (not (newline-marker-datum? (cadr current)))
              (not (comment-datum? (cadr current)))
            ) ;and
            (let ((info (call-with-values (lambda ()
                                            (reader-keyword-block-info current rest-indent format-reader-datum-inline)
                                          ) ;lambda
                          (lambda (block-count max-key-len) (list block-count max-key-len))
                        ) ;call-with-values
                  ) ;info
                 ) ;
              (let ((block-count (car info)) (max-key-len (cadr info)))
                (if (> block-count 0)
                  (reader-append-keyword-block current result rest-indent
                    prefix-ready? close-indent block-count max-key-len
                  ) ;reader-append-keyword-block
                  (reader-append-keyword-single current result rest-indent
                    prefix-ready? close-indent
                  ) ;reader-append-keyword-single
                ) ;if
              ) ;let
            ) ;let
           ) ;
           (else (reader-append-normal-item current result rest-indent
                   prefix-ready? close-indent
                 ) ;reader-append-normal-item
           ) ;else
          ) ;cond
        ) ;let
       ) ;
       ((null? current) (reader-append-close result close-indent))
       (else
         (reader-append-close
           (let* ((prefix (if prefix-ready? "" (string-append "\n" (spaces rest-indent))))
                  (before-tail (string-append result prefix ". "))
                 ) ;
             (string-append before-tail
               (format-reader-datum-at current (last-line-column before-tail))
             ) ;string-append
           ) ;let*
           close-indent
         ) ;reader-append-close
       ) ;else
      ) ;cond
    ) ;define

    (define (format-reader-pair-multiline datum indent)
      (if (not (pair? datum))
        (format-atom-value datum)
        (if (reader-fill-eligible? datum)
          (format-reader-pair-fill datum indent format-reader-datum-inline)
          (let* ((head (car datum))
                 (head-name (reader-head-name head))
                 (rule-head? (reader-rule-head? head))
                 (head-text (if rule-head?
                              (format-reader-datum-inline head)
                              (format-reader-datum-at head (+ indent 1))
                            ) ;if
                 ) ;head-text
                 (result (string-append "(" head-text))
                 (head-end-column (+ indent 1 (string-length head-text)))
                 (rest (cdr datum))
                 (selected (if rule-head?
                             (reader-select-first-line-items head-name rest head-end-column)
                             '()
                           ) ;if
                 ) ;selected
                 (after-selected (reader-skip-selected rest selected))
                 (with-selected (reader-append-selected result selected))
                 (body-indent (if rule-head?
                                (reader-rest-indent head-name indent selected after-selected)
                                (+ indent 1)
                              ) ;if
                 ) ;body-indent
                ) ;
            (reader-append-rest after-selected with-selected body-indent #f indent)
          ) ;let*
        ) ;if
      ) ;if
    ) ;define

    (define (format-reader-pair-at datum indent)
      (let ((candidate (format-reader-pair-inline datum)))
        (if
          (and (not (reader-datum-contains-newline-marker? datum))
            (not (string-contains? candidate "\n"))
            (<= (+ indent (string-length candidate)) max-inline-length)
          ) ;and
          candidate
          (format-reader-pair-multiline datum indent)
        ) ;if
      ) ;let
    ) ;define

    (define (format-reader-datum-inline datum)
      (cond ((raw-string-literal? datum) (raw-string-literal-source datum))
            ((char-literal? datum) (char-literal-source datum))
            ((dotted-tail? datum) (format-reader-datum-inline (dotted-tail-form datum)))
            ((raw-string-datum? datum) (cadr datum))
            ((comment-datum? datum) (format-comment-content (cadr datum)))
            ((and (stem-mode?) (quote-syntax-form? datum))
             (format-reader-pair-inline (cons 'quote (cdr datum)))
            ) ;
            ((and (not (stem-mode?)) (single-arg-symbol-form? datum 'quasiquote))
             (string-append "`" (format-reader-datum-inline (cadr datum)))
            ) ;
            ((and (not (stem-mode?)) (single-arg-symbol-form? datum 'unquote))
             (string-append "," (format-reader-datum-inline (cadr datum)))
            ) ;
            ((and (not (stem-mode?)) (single-arg-symbol-form? datum 'unquote-splicing))
             (string-append ",@" (format-reader-datum-inline (cadr datum)))
            ) ;
            ((and (not (stem-mode?)) (quote-syntax-form? datum))
             (string-append "'" (format-reader-datum-inline (cadr datum)))
            ) ;
            ((pair? datum) (format-reader-pair-inline datum))
            ((or (vector? datum) (byte-vector? datum)) (format-reader-vector-inline datum))
            (else (format-atom-value datum))
      ) ;cond
    ) ;define

    (define (format-reader-datum-at datum indent)
      (cond ((raw-string-literal? datum) (raw-string-literal-source datum))
            ((char-literal? datum) (char-literal-source datum))
            ((dotted-tail? datum) (format-reader-datum-at (dotted-tail-form datum) indent))
            ((raw-string-datum? datum) (cadr datum))
            ((comment-datum? datum) (format-comment-content (cadr datum)))
            ((and (stem-mode?) (quote-syntax-form? datum))
             (format-reader-pair-at (cons 'quote (cdr datum)) indent)
            ) ;
            ((and (not (stem-mode?)) (single-arg-symbol-form? datum 'quasiquote))
             (string-append "`" (format-reader-datum-at (cadr datum) (+ indent 1)))
            ) ;
            ((and (not (stem-mode?)) (single-arg-symbol-form? datum 'unquote))
             (string-append "," (format-reader-datum-at (cadr datum) (+ indent 1)))
            ) ;
            ((and (not (stem-mode?)) (single-arg-symbol-form? datum 'unquote-splicing))
             (string-append ",@" (format-reader-datum-at (cadr datum) (+ indent 2)))
            ) ;
            ((and (not (stem-mode?)) (quote-syntax-form? datum))
             (string-append "'" (format-reader-datum-at (cadr datum) (+ indent 1)))
            ) ;
            ((pair? datum) (format-reader-pair-at datum indent))
            ((or (vector? datum) (byte-vector? datum))
             (format-reader-vector-at datum indent)
            ) ;
            (else (format-atom-value datum))
      ) ;cond
    ) ;define

    (define (format-reader-datum datum)
      (format-reader-datum-at datum 0)
    ) ;define
  ) ;begin
) ;define-library
