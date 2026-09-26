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
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii go)
  (import (scheme base)
    (scheme case-lambda)
    (scheme time)
    (liii base)
    (liii error)
  ) ;import
  (export go go-worker-count make-chan chan? chan-send! chan-recv!
    chan-try-recv! chan-try-send! chan-close! chan-closed? select
  ) ;export
  (begin
    (define make-chan (case-lambda (() (g_make-chan 0)) ((cap) (g_make-chan cap))))

    (define (chan? obj)
      (g_chan? obj)
    ) ;define

    (define chan-send!
      (case-lambda
       ((ch val) (g_chan-send! ch val))
       ((ch val timeout-ms) (g_chan-send! ch val timeout-ms))
      ) ;case-lambda
    ) ;define

    (define chan-recv!
      (case-lambda
       ((ch) (g_chan-recv! ch))
       ((ch timeout-ms) (g_chan-recv! ch timeout-ms))
       ((ch timeout-ms default-val) (g_chan-recv! ch timeout-ms default-val))
      ) ;case-lambda
    ) ;define

    (define chan-try-recv!
      (case-lambda
       ((ch) (g_chan-try-recv! ch #f))
       ((ch default-val) (g_chan-try-recv! ch default-val))
      ) ;case-lambda
    ) ;define

    (define (chan-try-send! ch val)
      (g_chan-send! ch val 0)
    ) ;define

    (define (chan-close! ch)
      (g_chan-close! ch)
    ) ;define

    (define (chan-closed? ch)
      (g_chan-closed? ch)
    ) ;define

    (define (go-worker-count)
      (g_go-worker-count)
    ) ;define

    (define-macro (go vars . body)
      (if (list? vars)
        `(g_go-spawn (quote ,vars) (list ,@vars) (quote (begin ,@body)))
        `(g_go-spawn '() '() (quote (begin ,vars ,@body)))
      ) ;if
    ) ;define-macro

    (define-macro (select . clauses)
      (let ((default-branch #f) (timeout-branch #f) (timeout-ms 0) (cases '()))
        (for-each
          (lambda (clause)
            (cond
             ((and (pair? clause) (eq? (car clause) 'default))
              (set! default-branch (cdr clause))
             ) ;
             ((and (pair? clause) (eq? (car clause) 'timeout))
              (set! timeout-ms (cadr clause))
              (set! timeout-branch (cddr clause))
             ) ;
             ((and (pair? clause) (pair? (car clause))) (set! cases (cons clause cases)))
             (else (error 'syntax-error "select: invalid clause" clause))
            ) ;cond
          ) ;lambda
          clauses
        ) ;for-each
        (set! cases (reverse cases))

        (let ((poll-sym (gensym "poll")) (start-sym (gensym "start")))
          `(let* ((,start-sym (g_now-ms)))
             (let ,poll-sym
               ,()
               ,(let build-cases
                  ((rem cases))
                  (if (pair? rem)
                    (let* ((c (car rem))
                           (action (car c))
                           (body (cdr c))
                           (op (car action))
                           (next-step (build-cases (cdr rem))))
                      (cond ((eq? op 'chan-recv!)
                             (let ((ch-expr (cadr action))
                                   (var (caddr action))
                                   (tag (gensym "empty")))
                               `(let ((,var
                                       (chan-try-recv! ,ch-expr (quote ,tag))))
                                  (if (not (eq? ,var (quote ,tag)))
                                    (begin ,@body)
                                    ,next-step))))
                            ((eq? op 'chan-send!)
                             (let ((ch-expr (cadr action))
                                   (val-expr (caddr action)))
                               `(if (chan-try-send! ,ch-expr ,val-expr)
                                  (begin ,@body)
                                  ,next-step)))
                            (else (error 'syntax-error
                                    "select: unsupported channel operation"
                                    op))))
                    (cond (default-branch `(begin ,@default-branch))
                          (timeout-branch `(if (>= (- (g_now-ms) ,start-sym)
                                                 ,timeout-ms)
                                             (begin ,@timeout-branch)
                                             (begin (g_msleep 1) (,poll-sym))))
                          (else `(begin (g_msleep 1) (,poll-sym))))))))
        ) ;let
      ) ;let
    ) ;define-macro
  ) ;begin
) ;define-library
