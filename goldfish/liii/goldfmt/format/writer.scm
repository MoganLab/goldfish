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

(define-library (liii goldfmt format writer)
  (export make-writer writer-line writer-column writer-result emit-string!
    emit-newline! emit-spaces! selected-child selected-column
    select-first-line-children selected-count next-line-child-indent
    positioned-atom positioned-env emit-comment! emit-atom! emit-inline!
    format-inline try-inline can-inline? format-inline-atom-or-quote
    format-inline-atom-or-quote-at
  ) ;export
  (import (liii base)
    (liii list)
    (liii tree)
    (srfi srfi-13)
    (liii goldfmt record)
    (liii goldfmt rule)
    (liii goldfmt format atom)
    (liii goldfmt format reader)
  ) ;import
  (begin
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
                (string-append "("
                  (env-tag-name node)
                  (apply string-append
                    (map
                      (lambda (i)
                        (string-append (child-separator node i) (format-inline (vector-ref children i)))
                      ) ;lambda
                      (iota (vector-length children))
                    ) ;map
                  ) ;apply
                  ")"
                ) ;string-append
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
      (let ((newlines (string-count text #\newline))
            (last-newline (string-index-right text #\newline))
           ) ;
        (set-writer-line! writer (+ (writer-line writer) newlines))
        (set-writer-column! writer
          (if last-newline
            (- (string-length text) last-newline 1)
            (+ (writer-column writer) (string-length text))
          ) ;if
        ) ;set-writer-column!
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
      (let ((found (find (lambda (item) (env? (selected-child item))) selected)))
        (and found (selected-column found))
      ) ;let
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

    (define (format-inline-atom-or-quote value)
      (format-reader-datum value)
    ) ;define

    (define (format-inline-atom-or-quote-at value indent)
      (format-reader-datum-at value indent)
    ) ;define
  ) ;begin
) ;define-library
