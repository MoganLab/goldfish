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
    (srfi srfi-13)
    (liii goldfmt record)
    (liii goldfmt scan)
    (liii goldfmt format atom)
    (liii goldfmt format writer)
    (liii goldfmt format walk)
  ) ;import
  (begin
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
  ) ;begin
) ;define-library
