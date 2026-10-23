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

(define-library (liii goldfmt format walk)
  (export walk!)
  (import (liii base)
    (liii goldfmt record)
    (liii goldfmt rule)
    (liii goldfmt format atom)
    (liii goldfmt format writer)
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
) ;define-library
