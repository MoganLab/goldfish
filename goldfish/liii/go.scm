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
    chan-try-recv! chan-try-send! chan-close! chan-closed? select make-context
    make-timeout-context context? context-done? context-cancel! context-channel
    spawn-fiber fiber-yield! fiber-scheduler-run! make-fiber-chan fiber-chan?
    fiber-send! fiber-recv!
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

    (define-record-type <go-context>
      (%make-context done-chan)
      context?
      (done-chan context-channel)
    ) ;define-record-type

    (define (make-context)
      (%make-context (make-chan 1))
    ) ;define

    (define (context-done? ctx)
      (chan-closed? (context-channel ctx))
    ) ;define

    (define (context-cancel! ctx)
      (when (not (context-done? ctx))
        (chan-close! (context-channel ctx))
      ) ;when
    ) ;define

    (define (make-timeout-context ms)
      (let ((ctx (make-context)))
        (go (ctx ms) (g_msleep ms) (context-cancel! ctx))
        ctx
      ) ;let
    ) ;define

    ;; -----------------------------------------------------------------------
    ;; M:N 混合协程调度器：单 Session 内基于 call/cc 的用户态轻量协程调度引擎
    ;; -----------------------------------------------------------------------

    (define *ready-queue* '())
    (define *scheduler-return* #f)

    (define (enqueue-fiber! thunk)
      (set! *ready-queue* (append *ready-queue* (list thunk)))
    ) ;define

    (define (schedule-next!)
      (if (null? *ready-queue*)
        (if *scheduler-return* (*scheduler-return* #t) #f)
        (let ((next-thunk (car *ready-queue*)))
          (set! *ready-queue* (cdr *ready-queue*))
          (next-thunk)
        ) ;let
      ) ;if
    ) ;define

    (define (spawn-fiber thunk)
      (enqueue-fiber! (lambda () (thunk) (schedule-next!)))
    ) ;define

    (define (fiber-yield!)
      (call/cc
        (lambda (k) (enqueue-fiber! (lambda () (k #t))) (schedule-next!))
      ) ;call/cc
    ) ;define

    (define (fiber-scheduler-run!)
      (call/cc (lambda (exit-k) (set! *scheduler-return* exit-k) (schedule-next!)))
    ) ;define

    (define-record-type <fiber-chan>
      (%make-fiber-chan buffer waiting-receivers)
      fiber-chan?
      (buffer %fch-buf %fch-set-buf!)
      (waiting-receivers %fch-recv %fch-set-recv!)
    ) ;define-record-type

    (define (make-fiber-chan)
      (%make-fiber-chan '() '())
    ) ;define

    (define (fiber-send! fch val)
      (let ((recv-waiters (%fch-recv fch)))
        (if (pair? recv-waiters)
          (let ((receiver-k (car recv-waiters)))
            (%fch-set-recv! fch (cdr recv-waiters))
            (enqueue-fiber! (lambda () (receiver-k val)))
            (fiber-yield!)
          ) ;let
          (begin
            (%fch-set-buf! fch (append (%fch-buf fch) (list val)))
            (fiber-yield!)
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

    (define (fiber-recv! fch)
      (let ((buf (%fch-buf fch)))
        (if (pair? buf)
          (let ((val (car buf)))
            (%fch-set-buf! fch (cdr buf))
            val
          ) ;let
          (call/cc
            (lambda (k)
              (%fch-set-recv! fch (append (%fch-recv fch) (list k)))
              (schedule-next!)
            ) ;lambda
          ) ;call/cc
        ) ;if
      ) ;let
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
              (if default-branch
                (error 'syntax-error "select: multiple default clauses")
                (set! default-branch (cdr clause))
              ) ;if
             ) ;
             ((and (pair? clause) (eq? (car clause) 'timeout))
              (if timeout-branch
                (error 'syntax-error "select: multiple timeout clauses")
                (begin
                  (set! timeout-ms (cadr clause))
                  (set! timeout-branch (cddr clause))
                ) ;begin
              ) ;if
             ) ;
             ((and (pair? clause) (pair? (car clause))) (set! cases (cons clause cases)))
             (else (error 'syntax-error "select: invalid clause" clause))
            ) ;cond
          ) ;lambda
          clauses
        ) ;for-each

        (if (and default-branch timeout-branch)
          (error 'syntax-error "select: cannot specify both default and timeout clauses")
        ) ;if

        (set! cases (reverse cases))

        ;; 预绑定各个分支的通道与发送表达式，确保只求值一次（符合 Go 语义）
        (let ((poll-sym (gensym "poll"))
              (start-sym (gensym "start"))
              (pre-bindings '())
              (parsed-cases '())
             ) ;
          (for-each
            (lambda (c)
              (let* ((action (car c)) (body (cdr c)) (op (car action)))
                (cond
                 ((eq? op 'chan-recv!)
                  (let ((ch-sym (gensym "ch")))
                    (set! pre-bindings (cons `(,ch-sym ,(cadr action)) pre-bindings))
                    (set! parsed-cases (cons (list 'recv ch-sym (caddr action) body) parsed-cases))
                  ) ;let
                 ) ;
                 ((eq? op 'chan-send!)
                  (let ((ch-sym (gensym "ch")) (val-sym (gensym "val")))
                    (set! pre-bindings
                      (cons
                        `(,ch-sym ,(cadr action))
                        (cons
                          `(,val-sym ,(caddr action))
                          pre-bindings
                        ) ;cons
                      ) ;cons
                    ) ;set!
                    (set! parsed-cases (cons (list 'send ch-sym val-sym body) parsed-cases))
                  ) ;let
                 ) ;
                 (else (error 'syntax-error "select: unsupported channel operation" op))
                ) ;cond
              ) ;let*
            ) ;lambda
            cases
          ) ;for-each

          (set! pre-bindings (reverse pre-bindings))
          (set! parsed-cases (reverse parsed-cases))

          `(let* (,@pre-bindings (,start-sym (g_now-ms)))
             (let ,poll-sym
               ,()
               ,(let build-cases
                  ((rem parsed-cases))
                  (if (pair? rem)
                    (let* ((c (car rem))
                           (kind (car c))
                           (ch-sym (cadr c))
                           (next-step (build-cases (cdr rem))))
                      (if (eq? kind 'recv)
                        (let ((var (caddr c))
                              (body (cadddr c))
                              (tag (gensym "empty")))
                          `(let ((,var (chan-try-recv! ,ch-sym (quote ,tag))))
                             (if (not (eq? ,var (quote ,tag)))
                               (begin ,@body)
                               ,next-step)))
                        (let ((val-sym (caddr c)) (body (cadddr c)))
                          `(if (chan-try-send! ,ch-sym ,val-sym)
                             (begin ,@body)
                             ,next-step))))
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
