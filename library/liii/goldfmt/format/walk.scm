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
    (liii vector)
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
        (vector-every
          (lambda (child)
            (and (atom? child) (not (keyword-node? child)) (try-inline child))
          ) ;lambda
          children
        ) ;vector-every
      ) ;and
    ) ;define

    (define (newline-node-blank-lines node)
      (let ((children (env-children node)))
        (if (> (vector-length children) 0)
          (let ((child (vector-ref children 0)))
            (if (atom? child) (let ((val (atom-value child))) (if (number? val) val 1)) 1)
          ) ;let
          1
        ) ;if
      ) ;let
    ) ;define

    (define (walk-env-newline! node writer column)
      (let ((left-line (writer-line writer)))
        (emit-string! writer (make-newlines (newline-node-blank-lines node)))
        (positioned-env node column (vector) left-line (writer-line writer))
      ) ;let
    ) ;define

    (define (walk-env-quote! node writer column)
      (let ((left-line (writer-line writer)))
        (emit-string! writer "'")
        (emit-string! writer
          (format-inline-atom-or-quote-at (quote-env-content node) (+ column 1))
        ) ;emit-string!
        (positioned-env node column (vector) left-line (writer-line writer))
      ) ;let
    ) ;define

    (define (walk-env-reader-prefix! node writer column)
      (let ((left-line (writer-line writer)))
        (emit-string! writer (format-inline-atom-or-quote-at (env-value node) column))
        (positioned-env node column (vector) left-line (writer-line writer))
      ) ;let
    ) ;define

    (define (emit-close-tag! writer env-indent tag-name)
      (emit-newline! writer)
      (emit-spaces! writer env-indent)
      (emit-string! writer ") ;")
      (emit-string! writer tag-name)
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
                (emit-close-tag! writer env-indent tag-name)
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

    (define (emit-selected-children! writer node selected index result)
      (if (null? selected)
        result
        (begin
          (emit-string! writer (child-separator node index))
          (let ((new-child (walk! (selected-child (car selected)) writer (selected-column (car selected)))
                ) ;new-child
               ) ;
            (emit-selected-children! writer
              node
              (cdr selected)
              (+ index 1)
              (cons new-child result)
            ) ;emit-selected-children!
          ) ;let
        ) ;begin
      ) ;if
    ) ;define

    (define (keyword-pair-start? keyword-args? children i)
      (and keyword-args?
        (keyword-node? (vector-ref children i))
        (< (+ i 1) (vector-length children))
        (not (newline-node? (vector-ref children (+ i 1))))
        (not (comment-node? (vector-ref children (+ i 1))))
      ) ;and
    ) ;define

    (define (emit-keyword-pair! writer children i child-indent result)
      (emit-newline! writer)
      (emit-spaces! writer child-indent)
      (let* ((new-key (walk! (vector-ref children i) writer child-indent))
             (val (vector-ref children (+ i 1)))
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
            (list (+ i 2)
              (cons (walk! val writer (writer-column writer)) (cons new-key result))
            ) ;list
          ) ;begin
          (begin
            (emit-newline! writer)
            (emit-spaces! writer child-indent)
            (list (+ i 2) (cons (walk! val writer child-indent) (cons new-key result)))
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

    (define (emit-keyword-block-pairs! writer children j k block-count
              child-indent max-key-len result
            ) ;emit-keyword-block-pairs!
      (if (>= k block-count)
        (list j result)
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
              (emit-keyword-block-pairs! writer
                children
                (+ j 2)
                (+ k 1)
                block-count
                child-indent
                max-key-len
                (cons new-val (cons new-key result))
              ) ;emit-keyword-block-pairs!
            ) ;let
          ) ;let
        ) ;let
      ) ;if
    ) ;define

    (define (emit-keyword-section! writer children i child-indent result)
      (call-with-values (lambda () (compute-keyword-block children i child-indent))
        (lambda (block-count max-key-len)
          (if (> block-count 0)
            (emit-keyword-block-pairs! writer children i 0 block-count
              child-indent max-key-len result
            ) ;emit-keyword-block-pairs!
            (emit-keyword-pair! writer children i child-indent result)
          ) ;if
        ) ;lambda
      ) ;call-with-values
    ) ;define

    (define (emit-rest-children! writer children child-indent keyword-args? i result)
      (if (>= i (vector-length children))
        result
        (let ((child (vector-ref children i)))
          (cond ((newline-node? child)
                 (emit-newline! writer)
                 (emit-rest-children! writer children child-indent keyword-args? (+ i 1) result)
                ) ;
                ((keyword-pair-start? keyword-args? children i)
                 (let ((next (emit-keyword-section! writer children i child-indent result)))
                   (emit-rest-children! writer
                     children
                     child-indent
                     keyword-args?
                     (car next)
                     (cadr next)
                   ) ;emit-rest-children!
                 ) ;let
                ) ;
                (else (emit-newline! writer)
                  (emit-spaces! writer child-indent)
                  (let ((new-child (walk! child writer child-indent)))
                    (emit-rest-children! writer
                      children
                      child-indent
                      keyword-args?
                      (+ i 1)
                      (cons new-child result)
                    ) ;emit-rest-children!
                  ) ;let
                ) ;else
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (walk-env-general! node writer column)
      (let ((env-indent column) (left-line (writer-line writer)))
        (emit-string! writer "(")
        (emit-string! writer (env-tag-name node))
        (let* ((selected (select-first-line-children node (writer-column writer)))
               (rest-start (selected-count selected))
               (children (env-children node))
              ) ;
          (let ((head-children (emit-selected-children! writer node selected 0 '())))
            (let ((new-children
                    (if (< rest-start (vector-length children))
                      (let ((child-indent (next-line-child-indent node env-indent selected rest-start)))
                        (emit-rest-children! writer
                          children
                          child-indent
                          (not (no-keyword-args-form? (env-tag-name node)))
                          rest-start
                          head-children
                        ) ;emit-rest-children!
                      ) ;let
                      head-children
                    ) ;if
                  ) ;new-children
                 ) ;
              (emit-close-tag! writer env-indent (env-tag-name node))
              (positioned-env node
                env-indent
                (list->vector (reverse new-children))
                left-line
                (writer-line writer)
              ) ;positioned-env
            ) ;let
          ) ;let
        ) ;let*
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
      (cond ((newline-node? node) (walk-env-newline! node writer column))
            ((quote-env? node) (walk-env-quote! node writer column))
            ((reader-prefix-env? node) (walk-env-reader-prefix! node writer column))
            (else (if (fill-eligible-children? (env-children node))
                    (walk-env-fill! node writer column)
                    (walk-env-general! node writer column)
                  ) ;if
            ) ;else
      ) ;cond
    ) ;define
  ) ;begin
) ;define-library
