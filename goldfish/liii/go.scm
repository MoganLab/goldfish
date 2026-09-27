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
    (liii queue)
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
      ;; 使用 C++ 定时器到期关闭 done channel，不占用 worker 线程
      (let ((ctx (make-context)))
        (g_chan-timeout-close! (context-channel ctx) ms)
        ctx
      ) ;let
    ) ;define

    ;; -----------------------------------------------------------------------
    ;; M:N 混合协程调度器：单 Session 内基于 call/cc 的用户态轻量协程调度引擎
    ;; -----------------------------------------------------------------------

    (define *ready-queue* (make-list-queue (list)))
    (define *scheduler-return* #f)
    (define *suspended-fibers* 0)

    (define (enqueue-fiber! thunk)
      (list-queue-add-back! *ready-queue* thunk)
    ) ;define

    (define (schedule-next!)
      (if (list-queue-empty? *ready-queue*)
        (if (> *suspended-fibers* 0)
          ;; 就绪队列空但仍有挂起协程：全体死锁，报错而非静默退出
          (let ((n *suspended-fibers*))
            (set! *suspended-fibers* 0)
            (set! *scheduler-return* #f)
            (error 'deadlock "all fibers are blocked on channel operations" n)
          ) ;let
          (if *scheduler-return* (*scheduler-return* #t) #f)
        ) ;if
        (let ((next-thunk (list-queue-front *ready-queue*)))
          (list-queue-remove-front! *ready-queue*)
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
      (%make-fiber-chan (make-list-queue (list)) (make-list-queue (list)))
    ) ;define

    (define (fiber-send! fch val)
      (let ((recv-waiters (%fch-recv fch)))
        (if (list-queue-empty? recv-waiters)
          (list-queue-add-back! (%fch-buf fch) val)
          (let ((receiver-k (list-queue-front recv-waiters)))
            (list-queue-remove-front! recv-waiters)
            (set! *suspended-fibers* (- *suspended-fibers* 1))
            (enqueue-fiber! (lambda () (receiver-k val)))
          ) ;let
        ) ;if
      ) ;let
      (fiber-yield!)
    ) ;define

    (define (fiber-recv! fch)
      (let ((buf (%fch-buf fch)))
        (if (list-queue-empty? buf)
          (call/cc (lambda (k)
                     (list-queue-add-back! (%fch-recv fch) k)
                     (set! *suspended-fibers* (+ *suspended-fibers* 1))
                     (schedule-next!)
                   ) ;lambda
          ) ;call/cc
          (let ((val (list-queue-front buf)))
            (list-queue-remove-front! buf)
            val
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; (go (captured-vars ...) body ...) 在后台 worker 线程的独立 s7 会话中执行 body。
    ;; 注意：捕获变量只支持可序列化的数据类型（数字、字符串、符号、列表、vector、
    ;; bytevector、channel、let 等），不支持过程/闭包——传入函数会在 spawn 时
    ;; 抛 type-error。在 body 中直接引用全局函数名（如 car、display）即可，无需捕获。
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
        (let ((result-sym (gensym "result")) (pre-bindings '()) (parsed-cases '()))
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

          ;; 分为 recv / send 两组，交给 C++ wait-set（g_select）事件驱动阻塞等待
          (let ((recv-cases '()) (send-cases '()))
            (for-each
              (lambda (c)
                (if (eq? (car c) 'recv)
                  (set! recv-cases (append recv-cases (list c)))
                  (set! send-cases (append send-cases (list c)))
                ) ;if
              ) ;lambda
              parsed-cases
            ) ;for-each

            (let ((recv-dispatch
                    (let loop
                      ((rcs recv-cases) (i 0) (acc '()))
                      (if (null? rcs)
                        (reverse acc)
                        (loop (cdr rcs)
                          (+ i 1)
                          (cons
                            `((,i)
                              ((lambda (,(caddr (car rcs)))
                                 ,@(cadddr (car rcs)))
                               (vector-ref ,result-sym ,2)))
                            acc
                          ) ;cons
                        ) ;loop
                      ) ;if
                    ) ;let
                  ) ;recv-dispatch
                  (send-dispatch
                    (let loop
                      ((scs send-cases) (i 0) (acc '()))
                      (if (null? scs)
                        (reverse acc)
                        (loop (cdr scs) (+ i 1) (cons `((,i)
                                                        (begin
                                                          ,@(cadddr (car scs)))) acc))
                      ) ;if
                    ) ;let
                  ) ;send-dispatch
                 ) ;
              `(let* ,pre-bindings
                 (let ((,result-sym
                        (g_select (list ,@(map cadr recv-cases))
                          (list ,@(map (lambda (c) `(cons ,(cadr c) ,(caddr c)))
                                    send-cases))
                          ,(cond (default-branch 0)
                                 (timeout-branch timeout-ms)
                                 (else -1)))))
                   (if (not ,result-sym)
                     ,(cond (default-branch `(begin ,@default-branch))
                            (timeout-branch `(begin ,@timeout-branch))
                            ;; 无 default/timeout 时 g_select 无限等待，不会走到这里
                            (else '(begin)))
                     (case (vector-ref ,result-sym ,0)
                           ,@(if (pair? recv-cases)
                               `(((0)
                                  (case (vector-ref ,result-sym ,1)
                                        ,@recv-dispatch)))
                               '())
                           ,@(if (pair? send-cases)
                               `(((1)
                                  (case (vector-ref ,result-sym ,1)
                                        ,@send-dispatch)))
                               '())
                           (else (error 'fatal-error "select: unreachable"))))))
            ) ;let
          ) ;let
        ) ;let
      ) ;let
    ) ;define-macro
  ) ;begin
) ;define-library
