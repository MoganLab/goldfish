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

(define-library (liii goldfmt format)
  (export format-datum format-datum+node format-node format-string format-nodes
    format-inline can-inline?
  ) ;export
  (import (liii base)
    (liii goldfmt record)
    (liii goldfmt rule)
    (liii goldfmt scan)
    (liii syntax)
    (liii tree)
    (srfi srfi-13)
    (liii goldfmt format atom)
    (liii goldfmt format reader)
  ) ;import
  (begin
    (define (compute-keyword-block children start child-indent)
      (let ((len (vector-length children)))
        (let collect
          ((k start) (pairs '()))
          (define (finish)
            (keyword-block-prefix (reverse pairs) child-indent)
          ) ;define
          (if (>= (+ k 1) len)
            (finish)
            (let ((key (vector-ref children k)) (val (vector-ref children (+ k 1))))
              (if (and (keyword-node? key) (not (newline-node? val)) (not (comment-node? val)))
                (let ((val-inline (try-inline val)))
                  (if (string? val-inline)
                    (let ((key-len (keyword-node-len key)) (val-len (string-length val-inline)))
                      (if (<= (+ child-indent key-len 1 val-len) max-inline-length)
                        (collect (+ k 2) (cons (cons key-len val-len) pairs))
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
      ) ;let
    ) ;define

    (define (format-inline node)
      (cond ((comment-node? node) (format-comment-content (comment-content node)))
            ((newline-node? node) "")
            ((atom? node) (format-inline-atom-or-quote (atom-value node)))
            ((quote-env? node)
             (let ((content (quote-env-content node)))
               (string-append "'" (format-inline-atom-or-quote content))
             ) ;let
            ) ;
            ((reader-prefix-env? node) (format-inline-atom-or-quote (env-value node)))
            (else
              (let ((children (env-children node)))
                (let ((out (open-output-string)))
                  (display "(" out)
                  (display (env-tag-name node) out)
                  (let loop
                    ((i 0))
                    (if (>= i (vector-length children))
                      (begin
                        (display ")" out)
                        (get-output-string out)
                      ) ;begin
                      (begin
                        (display (child-separator node i) out)
                        (display (format-inline (vector-ref children i)) out)
                        (loop (+ i 1))
                      ) ;begin
                    ) ;if
                  ) ;let
                ) ;let
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    (define (first-child-env? node)
      (let ((children (env-children node)))
        (and (> (vector-length children) 0) (env? (vector-ref children 0)))
      ) ;let
    ) ;define

    (define (second-child-node node)
      (if (or (not (env? node)) (string=? (env-tag-name node) ""))
        #f
        (let ((children (env-children node)))
          (if (>= (vector-length children) 1) (vector-ref children 0) #f)
        ) ;let
      ) ;if
    ) ;define

    (define (node-datum node)
      (if (env? node) (env-value node) (atom-value node))
    ) ;define

    (define (second-child-tree-depth-exceeded? node)
      (let ((second (second-child-node node)))
        (and second
          (let ((datum (node-datum second)))
            (and datum
              (>= (tree-depth datum) (second-child-tree-depth-limit (env-tag-name node)))
            ) ;and
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (try-inline node)
      (cond ((comment-node? node) #t)
            ((newline-node? node) #f)
            ((atom? node)
             (let ((text (format-inline node)))
               (if (string-contains-newline? text) #f text)
             ) ;let
            ) ;
            ((contains-comment? node) #f)
            ((must-inline? (env-tag-name node)) #t)
            ((never-inline? (env-tag-name node)) #f)
            ((and (never-inline-when-first-child-env? (env-tag-name node))
               (first-child-env? node)
             ) ;and
             #f
            ) ;
            ((second-child-tree-depth-exceeded? node) #f)
            (else
              (let ((candidate (format-inline node)))
                (if (and (not (string-contains-newline? candidate))
                      (<= (string-length candidate) max-inline-length)
                    ) ;and
                  candidate
                  #f
                ) ;if
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    (define (can-inline? node)
      (if (try-inline node) #t #f)
    ) ;define

    (define (make-writer initial-column)
      (vector (open-output-string) 1 initial-column)
    ) ;define

    (define (writer-port writer)
      (vector-ref writer 0)
    ) ;define

    (define (writer-line writer)
      (vector-ref writer 1)
    ) ;define

    (define (writer-column writer)
      (vector-ref writer 2)
    ) ;define

    (define (set-writer-line! writer line)
      (vector-set! writer 1 line)
    ) ;define

    (define (set-writer-column! writer column)
      (vector-set! writer 2 column)
    ) ;define

    (define (writer-result writer)
      (get-output-string (writer-port writer))
    ) ;define

    (define (emit-string! writer text)
      (display text (writer-port writer))
      (let loop
        ((i 0) (line (writer-line writer)) (column (writer-column writer)))
        (if (>= i (string-length text))
          (begin
            (set-writer-line! writer line)
            (set-writer-column! writer column)
          ) ;begin
          (if (char=? (string-ref text i) #\newline)
            (loop (+ i 1) (+ line 1) 0)
            (loop (+ i 1) line (+ column 1))
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (emit-newline! writer)
      (display "\n" (writer-port writer))
      (set-writer-line! writer (+ (writer-line writer) 1))
      (set-writer-column! writer 0)
    ) ;define

    (define (emit-spaces! writer n)
      (if (> n 0)
        (begin
          (display (make-string n #\space) (writer-port writer))
          (set-writer-column! writer (+ (writer-column writer) n))
        ) ;begin
      ) ;if
    ) ;define

    (define (selected-child pair)
      (car pair)
    ) ;define

    (define (selected-column pair)
      (cdr pair)
    ) ;define

    (define (let-form? tag-name)
      (if (member tag-name '("let" "let*" "letrec" "letrec*" "let-values"
                             "let*-values"))
        #t
        #f
      ) ;if
    ) ;define

    (define (select-first-line-children node first-column)
      (if
        (and (second-child-tree-depth-exceeded? node)
          (not (let-form? (env-tag-name node)))
        ) ;and
        '()
        (let ((children (env-children node)) (tag-name (env-tag-name node)))
          (let ((limit (first-line-limit tag-name))
                (allow-child-env? (allow-first-line-child-env? tag-name))
                (no-kw-args? (no-keyword-args-form? tag-name))
               ) ;
            (let loop
              ((i 0) (column first-column) (direct-env-count 0) (result '()))
              (if (or (>= i (vector-length children)) (>= (length result) limit))
                (reverse result)
                (let* ((child (vector-ref children i))
                       (child-is-env? (env? child))
                       (next-direct-env-count (if child-is-env? (+ direct-env-count 1) direct-env-count)
                       ) ;next-direct-env-count
                      ) ;
                  (if (or (comment-node? child)
                        (newline-node? child)
                        (and (keyword-node? child) (not no-kw-args?))
                        (and child-is-env? (not allow-child-env?))
                        (> next-direct-env-count 1)
                      ) ;or
                    (reverse result)
                    (let* ((separator (child-separator node i))
                           (child-column (+ column (string-length separator)))
                           (next-result (cons (cons child child-column) result))
                           (inline-text (try-inline child))
                          ) ;
                      (if inline-text
                        (loop (+ i 1)
                          (+ child-column (string-length inline-text))
                          next-direct-env-count
                          next-result
                        ) ;loop
                        (reverse next-result)
                      ) ;if
                    ) ;let*
                  ) ;if
                ) ;let*
              ) ;if
            ) ;let
          ) ;let
        ) ;let
      ) ;if
    ) ;define

    (define (selected-count selected)
      (length selected)
    ) ;define

    (define (first-selected-env-column selected)
      (cond ((null? selected) #f)
            ((env? (selected-child (car selected))) (selected-column (car selected)))
            (else (first-selected-env-column (cdr selected)))
      ) ;cond
    ) ;define

    (define (by-first-rest-child-indent parent-indent rest-start children)
      (let ((first (vector-ref children rest-start)))
        (if (and (env? first) (string=? (env-tag-name first) ""))
          (+ parent-indent 1)
          (+ parent-indent 2)
        ) ;if
      ) ;let
    ) ;define

    (define (next-line-child-indent parent parent-indent selected rest-start)
      (let ((children (env-children parent))
            (strategy (rest-indent (env-tag-name parent)))
           ) ;
        (cond
         ((eq? strategy 'align-to-first-selected-env)
          (let ((column (first-selected-env-column selected)))
            (if column
              column
              (by-first-rest-child-indent parent-indent rest-start children)
            ) ;if
          ) ;let
         ) ;
         ((eq? strategy 'parent-plus2) (+ parent-indent 2))
         (else (by-first-rest-child-indent parent-indent rest-start children))
        ) ;cond
      ) ;let
    ) ;define

    (define (positioned-atom node indent left-line right-line)
      (make-atom
        :depth      (atom-depth node)
        :indent     indent
        :left-line  left-line
        :right-line right-line
        :value      (atom-value node)
      ) ;make-atom
    ) ;define

    (define (positioned-env node indent children left-line right-line)
      (make-env
        :tag-name   (env-tag-name node)
        :depth      (env-depth node)
        :indent     indent
        :children   children
        :left-line  left-line
        :right-line right-line
        :value      (env-value node)
      ) ;make-env
    ) ;define

    (define (emit-comment! node writer column)
      (let* ((left-line (writer-line writer))
             (content-node (vector-ref (env-children node) 0))
             (content (atom-value content-node))
             (content-indent (if (string=? content "") (+ column 2) (+ column 3)))
            ) ;
        (emit-string! writer (format-comment-content content))
        (positioned-env node
          column
          (vector (positioned-atom content-node content-indent left-line (writer-line writer))
          ) ;vector
          left-line
          (writer-line writer)
        ) ;positioned-env
      ) ;let*
    ) ;define

    (define (emit-atom! node writer column)
      (let ((left-line (writer-line writer)))
        (emit-string! writer (format-inline-atom-or-quote-at (atom-value node) column))
        (positioned-atom node column left-line (writer-line writer))
      ) ;let
    ) ;define

    (define (emit-inline! node writer column)
      (cond ((comment-node? node) (emit-comment! node writer column))
            ((atom? node)
             (let ((left-line (writer-line writer)))
               (emit-string! writer (format-inline-atom-or-quote (atom-value node)))
               (positioned-atom node column left-line (writer-line writer))
             ) ;let
            ) ;
            ((quote-env? node)
             (let ((left-line (writer-line writer)) (content (quote-env-content node)))
               (emit-string! writer "'")
               (emit-string! writer (format-inline-atom-or-quote-at content (+ column 1)))
               (positioned-env node column (vector) left-line (writer-line writer))
             ) ;let
            ) ;
            ((reader-prefix-env? node)
             (let ((left-line (writer-line writer)))
               (emit-string! writer (format-inline-atom-or-quote-at (env-value node) column))
               (positioned-env node column (vector) left-line (writer-line writer))
             ) ;let
            ) ;
            (else
              (let ((children (env-children node)))
                (let ((left-line (writer-line writer)))
                  (emit-string! writer "(")
                  (emit-string! writer (env-tag-name node))
                  (let loop
                    ((i 0) (new-children '()))
                    (if (>= i (vector-length children))
                      (begin
                        (emit-string! writer ")")
                        (positioned-env node
                          column
                          (list->vector (reverse new-children))
                          left-line
                          (writer-line writer)
                        ) ;positioned-env
                      ) ;begin
                      (begin
                        (emit-string! writer (child-separator node i))
                        (let ((new-child (emit-inline! (vector-ref children i) writer (writer-column writer)))
                             ) ;
                          (loop (+ i 1) (cons new-child new-children))
                        ) ;let
                      ) ;begin
                    ) ;if
                  ) ;let
                ) ;let
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    (define (fill-eligible-children? children)
      (and (>= (vector-length children) fill-min-children)
        (let loop
          ((i 0))
          (cond ((>= i (vector-length children)) #t)
                ((let ((child (vector-ref children i)))
                   (and (atom? child) (not (keyword-node? child)) (try-inline child))
                 ) ;let
                 (loop (+ i 1))
                ) ;
                (else #f)
          ) ;cond
        ) ;let
      ) ;and
    ) ;define

    (define (walk-env-fill! node writer column)
      (let ((env-indent column)
            (left-line (writer-line writer))
            (children (env-children node))
            (tag-name (env-tag-name node))
           ) ;
        (emit-string! writer "(")
        (emit-string! writer tag-name)
        (let ((rest-indent (+ env-indent (if (string=? tag-name "") 1 2))))
          (let loop
            ((i 0) (new-children '()))
            (if (>= i (vector-length children))
              (begin
                (emit-newline! writer)
                (emit-spaces! writer env-indent)
                (emit-string! writer ") ;")
                (emit-string! writer tag-name)
                (positioned-env node
                  env-indent
                  (list->vector (reverse new-children))
                  left-line
                  (writer-line writer)
                ) ;positioned-env
              ) ;begin
              (let* ((child (vector-ref children i))
                     (text (try-inline child))
                     (separator (child-separator node i))
                     (needed (+ (string-length separator) (string-length text)))
                     (wrap?
                       (and (> i 0) (> (+ (writer-column writer) needed) max-inline-length))
                     ) ;wrap?
                    ) ;
                (if wrap?
                  (begin
                    (emit-newline! writer)
                    (emit-spaces! writer rest-indent)
                  ) ;begin
                  (emit-string! writer separator)
                ) ;if
                (let ((child-column (writer-column writer)))
                  (emit-string! writer text)
                  (loop (+ i 1)
                    (cons (positioned-atom child child-column (writer-line writer) (writer-line writer))
                      new-children
                    ) ;cons
                  ) ;loop
                ) ;let
              ) ;let*
            ) ;if
          ) ;let
        ) ;let
      ) ;let
    ) ;define

    (define (walk! node writer column)
      (cond ((comment-node? node) (emit-comment! node writer column))
            ((atom? node) (emit-atom! node writer column))
            ((can-inline? node) (emit-inline! node writer column))
            (else (walk-env! node writer column))
      ) ;cond
    ) ;define

    (define (walk-env! node writer column)
      (let ((env-indent column) (left-line (writer-line writer)))
        (cond
         ((newline-node? node)
          (let ((blank-lines
                  (if (> (vector-length (env-children node)) 0)
                    (let ((child (vector-ref (env-children node) 0)))
                      (if (atom? child) (let ((val (atom-value child))) (if (number? val) val 1)) 1)
                    ) ;let
                    1
                  ) ;if
                ) ;blank-lines
               ) ;
            (emit-string! writer (make-newlines blank-lines))
            (positioned-env node env-indent (vector) left-line (writer-line writer))
          ) ;let
         ) ;
         ((quote-env? node)
          (let ((content (quote-env-content node)))
            (emit-string! writer "'")
            (emit-string! writer (format-inline-atom-or-quote-at content (+ env-indent 1)))
            (positioned-env node env-indent (vector) left-line (writer-line writer))
          ) ;let
         ) ;
         ((reader-prefix-env? node)
          (emit-string! writer
            (format-inline-atom-or-quote-at (env-value node) env-indent)
          ) ;emit-string!
          (positioned-env node env-indent (vector) left-line (writer-line writer))
         ) ;
         (else
           (if (fill-eligible-children? (env-children node))
             (walk-env-fill! node writer column)
             (begin
               (emit-string! writer "(")
               (emit-string! writer (env-tag-name node))
               (let* ((first-column (writer-column writer))
                      (selected (select-first-line-children node first-column))
                      (rest-start (selected-count selected))
                      (children (env-children node))
                     ) ;
                 (let ((new-children
                         (let loop-selected
                           ((items selected) (index 0) (result '()))
                           (if (null? items)
                             result
                             (begin
                               (emit-string! writer (child-separator node index))
                               (let ((new-child (walk! (selected-child (car items)) writer (selected-column (car items)))
                                     ) ;new-child
                                    ) ;
                                 (loop-selected (cdr items) (+ index 1) (cons new-child result))
                               ) ;let
                             ) ;begin
                           ) ;if
                         ) ;let
                       ) ;new-children
                      ) ;
                   (let ((new-children
                           (if (< rest-start (vector-length children))
                             (let ((child-indent (next-line-child-indent node env-indent selected rest-start)))
                               (let ((keyword-args? (not (no-keyword-args-form? (env-tag-name node)))))
                                 (let loop-rest
                                   ((i rest-start) (result new-children))
                                   (if (>= i (vector-length children))
                                     result
                                     (let ((child (vector-ref children i)))
                                       (cond
                                        ((newline-node? child)
                                         (begin
                                           (emit-newline! writer)
                                           (loop-rest (+ i 1) result)
                                         ) ;begin
                                        ) ;
                                        ((and keyword-args?
                                           (keyword-node? child)
                                           (< (+ i 1) (vector-length children))
                                           (not (newline-node? (vector-ref children (+ i 1))))
                                           (not (comment-node? (vector-ref children (+ i 1))))
                                         ) ;and
                                         (call-with-values (lambda () (compute-keyword-block children i child-indent))
                                           (lambda (block-count max-key-len)
                                             (if (> block-count 0)
                                               (let emit-block
                                                 ((j i) (k 0) (res result))
                                                 (if (>= k block-count)
                                                   (loop-rest j res)
                                                   (let ((key-node (vector-ref children j)) (val-node (vector-ref children (+ j 1))))
                                                     (emit-newline! writer)
                                                     (emit-spaces! writer child-indent)
                                                     (let ((new-key (walk! key-node writer child-indent)))
                                                       (emit-string! writer
                                                         (spaces
                                                           (+ (- max-key-len (- (writer-column writer) child-indent)) 1)
                                                         ) ;spaces
                                                       ) ;emit-string!
                                                       (let ((new-val (walk! val-node writer (writer-column writer))))
                                                         (emit-block (+ j 2) (+ k 1) (cons new-val (cons new-key res)))
                                                       ) ;let
                                                     ) ;let
                                                   ) ;let
                                                 ) ;if
                                               ) ;let
                                               (let* ((val (vector-ref children (+ i 1)))
                                                      (_ (emit-newline! writer))
                                                      (_ (emit-spaces! writer child-indent))
                                                      (new-key (walk! child writer child-indent))
                                                      (val-inline (try-inline val))
                                                      (fits-inline?
                                                        (and (string? val-inline)
                                                          (<= (+ (writer-column writer) 1 (string-length val-inline)) max-inline-length)
                                                        ) ;and
                                                      ) ;fits-inline?
                                                     ) ;
                                                 (if fits-inline?
                                                   (begin
                                                     (emit-string! writer " ")
                                                     (let ((new-val (walk! val writer (writer-column writer))))
                                                       (loop-rest (+ i 2) (cons new-val (cons new-key result)))
                                                     ) ;let
                                                   ) ;begin
                                                   (begin
                                                     (emit-newline! writer)
                                                     (emit-spaces! writer child-indent)
                                                     (let ((new-val (walk! val writer child-indent)))
                                                       (loop-rest (+ i 2) (cons new-val (cons new-key result)))
                                                     ) ;let
                                                   ) ;begin
                                                 ) ;if
                                               ) ;let*
                                             ) ;if
                                           ) ;lambda
                                         ) ;call-with-values
                                        ) ;
                                        (else
                                          (begin
                                            (emit-newline! writer)
                                            (emit-spaces! writer child-indent)
                                            (let ((new-child (walk! child writer child-indent)))
                                              (loop-rest (+ i 1) (cons new-child result))
                                            ) ;let
                                          ) ;begin
                                        ) ;else
                                       ) ;cond
                                     ) ;let
                                   ) ;if
                                 ) ;let
                               ) ;let
                             ) ;let
                             new-children
                           ) ;if
                         ) ;new-children
                        ) ;
                     (emit-newline! writer)
                     (emit-spaces! writer env-indent)
                     (emit-string! writer ") ;")
                     (emit-string! writer (env-tag-name node))
                     (positioned-env node
                       env-indent
                       (list->vector (reverse new-children))
                       left-line
                       (writer-line writer)
                     ) ;positioned-env
                   ) ;let
                 ) ;let
               ) ;let*
             ) ;begin
           ) ;if
         ) ;else
        ) ;cond
      ) ;let
    ) ;define
  ) ;begin
  (define (format-node node column)
    (let ((writer (make-writer column)))
      (let ((new-node (walk! node writer column)))
        (values (writer-result writer) new-node)
      ) ;let
    ) ;let
  ) ;define

  (define (format-datum+node datum)
    (format-node (scan datum) 0)
  ) ;define

  (define (format-datum datum)
    (call-with-values (lambda () (format-datum+node datum))
      (lambda (text new-node) text)
    ) ;call-with-values
  ) ;define

  (define (read-all port)
    (let loop
      ((result '()))
      (let ((datum (read port)))
        (if (eof-object? datum) (reverse result) (loop (cons datum result)))
      ) ;let
    ) ;let
  ) ;define

  (define (join-top-level pieces)
    (let ((out (open-output-string)))
      (let loop
        ((rest pieces) (first #t))
        (cond
         ((null? rest)
          (let ((result (get-output-string out)))
            (if (and (not (string=? result "")) (not (string-suffix? "\n" result)))
              (string-append result "\n")
              result
            ) ;if
          ) ;let
         ) ;
         (first (display (car rest) out) (loop (cdr rest) #f))
         (else (display "\n" out) (display (car rest) out) (loop (cdr rest) #f))
        ) ;cond
      ) ;let
    ) ;let
  ) ;define

  (define (define-form? datum)
    (and (pair? datum) (not (null? datum)) (eq? (car datum) 'define))
  ) ;define

  (define (define-node? node)
    (and (env? node) (string=? (env-tag-name node) "define"))
  ) ;define

  (define (newline-form? datum)
    (and (pair? datum)
      (not (null? datum))
      (eq? (car datum) '*newline*)
      (not (null? (cdr datum)))
      (number? (cadr datum))
    ) ;and
  ) ;define

  (define (newline-form-count datum)
    (cadr datum)
  ) ;define

  (define (needs-blank-line? datum is-first-expr)
    (and (define-form? datum) (not is-first-expr))
  ) ;define

  (define (format-top-level datums)
    (let loop
      ((rest datums) (is-first #t) (result '()))
      (if (null? rest)
        (reverse result)
        (let ((datum (car rest)))
          (cond
           ((newline-form? datum)
            (if is-first
              (loop (cdr rest) #t result)
              (let ((blank-lines (newline-form-count datum)))
                (loop (cdr rest) #f (cons (make-newlines blank-lines) result))
              ) ;let
            ) ;if
           ) ;
           (else
             (let* ((needs-blank (needs-blank-line? datum is-first))
                    (formatted (format-datum datum))
                    (new-result (if needs-blank (cons formatted (cons "" result)) (cons formatted result))
                    ) ;new-result
                   ) ;
               (loop (cdr rest) #f new-result)
             ) ;let*
           ) ;else
          ) ;cond
        ) ;let
      ) ;if
    ) ;let
  ) ;define

  (define (format-top-level-nodes nodes)
    (let loop
      ((i 0) (is-first #t) (result '()))
      (if (>= i (vector-length nodes))
        (reverse result)
        (let ((node (vector-ref nodes i)))
          (if (newline-node? node)
            (loop (+ i 1) #f (cons (make-newlines (newline-count node)) result))
            (call-with-values (lambda () (format-node node 0))
              (lambda (formatted positioned-node)
                (let* ((prev-node (if (> i 0) (vector-ref nodes (- i 1)) #f))
                       (needs-blank
                         (and (define-node? node)
                           (not is-first)
                           (not (and prev-node (newline-node? prev-node)))
                         ) ;and
                       ) ;needs-blank
                       (next-result (if needs-blank (cons formatted (cons "" result)) (cons formatted result))
                       ) ;next-result
                      ) ;
                  (loop (+ i 1) #f next-result)
                ) ;let*
              ) ;lambda
            ) ;call-with-values
          ) ;if
        ) ;let
      ) ;if
    ) ;let
  ) ;define

  (define (format-string source)
    (let* ((nodes (scan-source-string source)) (pieces (format-top-level-nodes nodes)))
      (join-top-level pieces)
    ) ;let*
  ) ;define

  (define (format-nodes nodes)
    (join-top-level (format-top-level-nodes nodes))
  ) ;define
) ;define-library
