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
  (export format-reader-datum format-reader-datum-at format-reader-datum-inline
    format-inline-atom-or-quote format-inline-atom-or-quote-at fill-min-children
  ) ;export
  (import (liii base)
    (liii goldfmt record)
    (liii goldfmt rule)
    (liii goldfmt format atom)
    (liii goldfmt scan normalize)
    (srfi srfi-13)
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

    (define (format-inline-atom-or-quote value)
      (format-reader-datum value)
    ) ;define

    (define (format-inline-atom-or-quote-at value indent)
      (format-reader-datum-at value indent)
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

    (define (reader-format-selected-item item column)
      (format-reader-datum-at item column)
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
                       (item-text (reader-format-selected-item item item-column))
                       (next-result (cons (list item item-column item-text) result))
                       (next-column (+ item-column (string-length item-text)))
                      ) ;
                  (if (string-contains-newline? item-text)
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

    (define (format-reader-vector-inline datum)
      (let ((prefix (if (byte-vector? datum) "#u8(" "#(")))
        (let loop
          ((i 0) (pieces '()))
          (if (>= i (vector-length datum))
            (string-append prefix (string-join (reverse pieces) " ") ")")
            (loop (+ i 1) (cons (format-reader-datum-inline (vector-ref datum i)) pieces))
          ) ;if
        ) ;let
      ) ;let
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

    (define (format-reader-vector-multiline datum indent)
      (let* ((prefix (reader-vector-prefix datum))
             (item-indent (+ indent (string-length prefix)))
             (close-marker (string-append "\n" (spaces indent) ") ;#"))
            ) ;
        (let loop
          ((items (reader-vector->list datum)) (pieces (list prefix)) (prefix-ready? #t))
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
              (not (string-contains-newline? candidate))
              (<= (+ indent (string-length candidate)) max-inline-length)
            ) ;and
          ) ;or
          candidate
          (format-reader-vector-multiline datum indent)
        ) ;if
      ) ;let
    ) ;define

    (define (format-reader-vector datum)
      (format-reader-vector-at datum 0)
    ) ;define

    (define (format-reader-pair-inline datum)
      (let loop
        ((current datum) (pieces '()))
        (cond
         ((pair? current)
          (loop (cdr current) (cons (format-reader-datum-inline (car current)) pieces))
         ) ;
         ((null? current) (string-append "(" (string-join (reverse pieces) " ") ")"))
         (else (string-append "("
                 (string-join (reverse pieces) " ")
                 " . "
                 (format-reader-datum-inline current)
                 ")"
               ) ;string-append
         ) ;else
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

    (define (format-reader-pair-fill datum indent)
      (let* ((head (car datum))
             (rule-head? (reader-rule-head? head))
             (rest-indent (+ indent (if rule-head? 2 1)))
            ) ;
        (let loop
          ((current datum) (column (+ indent 1)) (first? #t) (pieces (list "(")))
          (if (null? current)
            (apply string-append (reverse (cons ")" pieces)))
            (let* ((item (car current))
                   (text (format-reader-datum-inline item))
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

    (define (reader-append-close result close-indent)
      (if (string-suffix? ";#" result)
        (string-append result "\n" (spaces close-indent) ")")
        (string-append result ")")
      ) ;if
    ) ;define

    (define (reader-keyword-block-info current rest-indent)
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
              (let ((val-inline (format-reader-datum-inline val)))
                (if (not (string-contains-newline? val-inline))
                  (let ((key-len (string-length (format-reader-datum-inline key)))
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
            (call-with-values (lambda () (reader-keyword-block-info current rest-indent))
              (lambda (block-count max-key-len)
                (if (> block-count 0)
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
                  (let* ((key-text (format-reader-datum-inline item))
                         (key-len (string-length key-text))
                         (val-item (cadr current))
                         (key-line-prefix (if prefix-ready? "" (string-append "\n" (spaces rest-indent)))
                         ) ;key-line-prefix
                         (val-inline (format-reader-datum-inline val-item))
                         (fits-inline?
                           (and (not (string-contains-newline? val-inline))
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
                      (let ((val-text (format-reader-datum-at val-item rest-indent)))
                        (reader-append-rest (cddr current)
                          (string-append result
                            key-line-prefix
                            key-text
                            "\n"
                            (spaces rest-indent)
                            val-text
                          ) ;string-append
                          rest-indent
                          #f
                          close-indent
                        ) ;reader-append-rest
                      ) ;let
                    ) ;if
                  ) ;let*
                ) ;if
              ) ;lambda
            ) ;call-with-values
           ) ;
           (else
             (let ((is-last-comment? (and (null? (cdr current)) (comment-datum? item)))
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
             ) ;let
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
          (format-reader-pair-fill datum indent)
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
            (not (string-contains-newline? candidate))
            (<= (+ indent (string-length candidate)) max-inline-length)
          ) ;and
          candidate
          (format-reader-pair-multiline datum indent)
        ) ;if
      ) ;let
    ) ;define

    (define (format-reader-pair datum)
      (format-reader-pair-at datum 0)
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
