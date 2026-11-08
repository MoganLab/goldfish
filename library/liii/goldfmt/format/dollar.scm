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

(define-library (liii goldfmt format dollar)
  (export walk-env-dollar!)
  (import (liii base)
    (liii vector)
    (liii goldfmt record)
    (liii goldfmt format atom)
    (liii goldfmt format writer)
  ) ;import
  (begin
    (define (emit-close-dollar! writer env-indent node left-line new-children)
      (emit-newline! writer)
      (emit-spaces! writer env-indent)
      (emit-string! writer ") ;$")
      (positioned-env node
        env-indent
        (list->vector (reverse new-children))
        left-line
        (writer-line writer)
      ) ;positioned-env
    ) ;define

    (define (consume-dollar-args! writer children len j op-children walk!)
      (if (or (>= j len)
            (keyword-node? (vector-ref children j))
            (comment-node? (vector-ref children j))
            (newline-node? (vector-ref children j))
          ) ;or
        (values j op-children)
        (let ((arg (vector-ref children j)))
          (emit-string! writer " ")
          (let ((new-arg (walk! arg writer (writer-column writer))))
            (consume-dollar-args! writer
              children
              len
              (+ j 1)
              (cons new-arg op-children)
              walk!
            ) ;consume-dollar-args!
          ) ;let
        ) ;let
      ) ;if
    ) ;define

    (define (walk-dollar-keyword! writer children len i data-column new-children
              walk! env-indent node left-line
            ) ;walk-dollar-keyword!
      (emit-newline! writer)
      (emit-spaces! writer data-column)
      (let ((new-key (walk! (vector-ref children i) writer data-column)))
        (call-with-values (lambda ()
                            (consume-dollar-args! writer children len (+ i 1) (list new-key) walk!)
                          ) ;lambda
          (lambda (next-j op-children)
            (walk-dollar-loop! writer
              children
              len
              next-j
              data-column
              (append op-children new-children)
              walk!
              env-indent
              node
              left-line
            ) ;walk-dollar-loop!
          ) ;lambda
        ) ;call-with-values
      ) ;let
    ) ;define

    (define (walk-dollar-loop! writer children len i data-column new-children
              walk! env-indent node left-line
            ) ;walk-dollar-loop!
      (if (>= i len)
        (emit-close-dollar! writer env-indent node left-line new-children)
        (let ((child (vector-ref children i)))
          (cond ((newline-node? child)
                 (emit-newline! writer)
                 (walk-dollar-loop! writer
                   children
                   len
                   (+ i 1)
                   data-column
                   new-children
                   walk!
                   env-indent
                   node
                   left-line
                 ) ;walk-dollar-loop!
                ) ;
                ((comment-node? child)
                 (emit-newline! writer)
                 (emit-spaces! writer data-column)
                 (let ((new-c (walk! child writer data-column)))
                   (walk-dollar-loop! writer
                     children
                     len
                     (+ i 1)
                     data-column
                     (cons new-c new-children)
                     walk!
                     env-indent
                     node
                     left-line
                   ) ;walk-dollar-loop!
                 ) ;let
                ) ;
                ((keyword-node? child)
                 (walk-dollar-keyword! writer children len i data-column
                   new-children walk! env-indent node left-line
                 ) ;walk-dollar-keyword!
                ) ;
                (else (emit-newline! writer)
                  (emit-spaces! writer data-column)
                  (let ((new-c (walk! child writer data-column)))
                    (walk-dollar-loop! writer
                      children
                      len
                      (+ i 1)
                      data-column
                      (cons new-c new-children)
                      walk!
                      env-indent
                      node
                      left-line
                    ) ;walk-dollar-loop!
                  ) ;let
                ) ;else
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (walk-dollar-data! writer children len k env-indent head-children
              walk! node left-line
            ) ;walk-dollar-data!
      (if (>= k len)
        (emit-close-dollar! writer env-indent node left-line head-children)
        (let ((c (vector-ref children k)))
          (cond ((newline-node? c)
                 (emit-newline! writer)
                 (walk-dollar-data! writer
                   children
                   len
                   (+ k 1)
                   env-indent
                   head-children
                   walk!
                   node
                   left-line
                 ) ;walk-dollar-data!
                ) ;
                ((comment-node? c)
                 (emit-newline! writer)
                 (emit-spaces! writer (+ env-indent 3))
                 (let ((new-c (walk! c writer (+ env-indent 3))))
                   (walk-dollar-data! writer
                     children
                     len
                     (+ k 1)
                     env-indent
                     (cons new-c head-children)
                     walk!
                     node
                     left-line
                   ) ;walk-dollar-data!
                 ) ;let
                ) ;
                (else (emit-string! writer " ")
                  (let* ((data-column (writer-column writer)) (new-data (walk! c writer data-column)))
                    (walk-dollar-loop! writer
                      children
                      len
                      (+ k 1)
                      data-column
                      (cons new-data head-children)
                      walk!
                      env-indent
                      node
                      left-line
                    ) ;walk-dollar-loop!
                  ) ;let*
                ) ;else
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (walk-env-dollar! node writer column walk!)
      (let ((env-indent column)
            (left-line (writer-line writer))
            (children (env-children node))
            (len (vector-length (env-children node)))
           ) ;
        (emit-string! writer "(")
        (emit-string! writer "$")
        (walk-dollar-data! writer children len 0 env-indent '() walk! node left-line)
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
