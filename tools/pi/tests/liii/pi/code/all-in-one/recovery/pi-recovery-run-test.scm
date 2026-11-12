;; pi-recovery-run
;; 全链枯竭时的步进式等待、取消与到期重试引擎。
;;
;; 语法
;; ----
;; (pi-recovery-run attempt-fn aio-cfg options)
;;
;; 参数
;; ----
;; attempt-fn : procedure
;; 接受等待期间新增 steering 文本列表，重新执行一次既有链式请求。
;; aio-cfg : all-in-one-cfg?
;; 链式配置，提供 continuous、maxWaitMs 与 retryIntervalMs。
;; options : alist
;; 可选的 now-fn、sleep-fn、aborted?、poll-steering、emit、
;; chain-id、config-dir 与 step-ms 钩子。
;;
;; 返回值
;; ----
;; assistant message
;; 恢复成功返回真实响应；取消返回 aborted；预算耗尽返回 error 消息。

(import (scheme base)
  (liii check)
  (liii path)
  (liii shutil)
  (liii os)
  (liii string)
  (liii pi ai message)
  (liii pi code all-in-one types)
  (liii pi code all-in-one alarm)
  (liii pi code all-in-one recovery)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "pi-recovery-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)

;; 1. continuous = #f 时立即返回全链枯竭错误，不休眠、不调用 attempt-fn
(let* ((cfg (make-all-in-one-cfg #f 10000 1000 8000 '()))
       (attempt-called #f)
       (sleep-called #f)
       (resp
         (pi-recovery-run (lambda (st) (set! attempt-called #t) 'all-nodes-exhausted)
           cfg
           (list (cons 'sleep-fn (lambda (ms) (set! sleep-called #t))))
         ) ;pi-recovery-run
       ) ;resp
      ) ;
  (check-false attempt-called)
  (check-false sleep-called)
  (check (pi-assistant-message-stop-reason resp) => "error")
  (check-true (string? (pi-assistant-message-error-message resp)))
) ;let*

;; 2. 注入 aborted? = #t 时及时退出并返回 aborted 消息
(let* ((cfg (make-all-in-one-cfg #t 10000 1000 8000 '()))
       (events '())
       (now-ms 1000)
       (resp
         (pi-recovery-run (lambda (st) 'all-nodes-exhausted)
           cfg
           (list (cons 'now-fn (lambda () now-ms))
             (cons 'sleep-fn (lambda (ms) (set! now-ms (+ now-ms ms))))
             (cons 'aborted? (lambda () #t))
             (cons 'emit (lambda (ev . args) (set! events (cons (cons ev args) events))))
             (cons 'chain-id "test-chain")
           ) ;list
         ) ;pi-recovery-run
       ) ;resp
      ) ;
  (check (pi-assistant-message-stop-reason resp) => "aborted")
  (check-true (pair? (assoc 'recovery_end events)))
  (check (list-ref (assoc 'recovery_end events) 2) => 'aborted)
) ;let*

;; 3. 第一次到期重试仍枯竭，第二次到期重试成功自愈
(let* ((cfg (make-all-in-one-cfg #t 10000 500 8000 '()))
       (now-ms 1000)
       (attempts 0)
       (events '())
       (resp
         (pi-recovery-run (lambda (steering)
                            (set! attempts (+ attempts 1))
                            (if (= attempts 1)
                              'all-nodes-exhausted
                              (pi-assistant-message '() "stop" #f #f #f "provider" "model")
                            ) ;if
                          ) ;lambda
           cfg
           (list (cons 'now-fn (lambda () now-ms))
             (cons 'sleep-fn (lambda (ms) (set! now-ms (+ now-ms ms))))
             (cons 'aborted? (lambda () #f))
             (cons 'emit (lambda (ev . args) (set! events (cons (cons ev args) events))))
             (cons 'chain-id "coding-fast")
             (cons 'step-ms 100)
           ) ;list
         ) ;pi-recovery-run
       ) ;resp
      ) ;
  (check attempts => 2)
  (check (pi-assistant-message-stop-reason resp) => "stop")
  ;; 验证事件序列中包含 recovery_wait、recovery_retry 与 recovery_end
  (check-true (pair? (assoc 'recovery_wait events)))
  (check-true (pair? (assoc 'recovery_retry events)))
  (check (list-ref (assoc 'recovery_end events) 2) => 'success)
) ;let*

;; 4. 等待期间收到 steering 消息：立即唤醒重试并合并新消息
(let* ((cfg (make-all-in-one-cfg #t 10000 2000 8000 '()))
       (now-ms 1000)
       (received-steering '())
       (steering-queue (list "steer-msg-1"))
       (resp
         (pi-recovery-run (lambda (steering)
                            (set! received-steering steering)
                            (pi-assistant-message '() "stop")
                          ) ;lambda
           cfg
           (list (cons 'now-fn (lambda () now-ms))
             (cons 'sleep-fn (lambda (ms) (set! now-ms (+ now-ms ms))))
             (cons 'poll-steering
               (lambda ()
                 (if (pair? steering-queue)
                   (let ((m (car steering-queue)))
                     (set! steering-queue (cdr steering-queue))
                     (list m)
                   ) ;let
                   '()
                 ) ;if
               ) ;lambda
             ) ;cons
             (cons 'chain-id "coding-fast")
           ) ;list
         ) ;pi-recovery-run
       ) ;resp
      ) ;
  (check (pi-assistant-message-stop-reason resp) => "stop")
  (check received-steering => '("steer-msg-1"))
) ;let*

;; 5. 多次尝试累计超过 maxWaitMs：返回超时错误并写入带有 wait-timeout 的 EXHAUSTED 日志
(let* ((cfg (make-all-in-one-cfg #t 3000 1000 8000 '()))
       (now-ms 1000)
       (attempts 0)
       (events '())
       (resp
         (pi-recovery-run (lambda (steering) (set! attempts (+ attempts 1)) 'all-nodes-exhausted)
           cfg
           (list (cons 'now-fn (lambda () now-ms))
             (cons 'sleep-fn (lambda (ms) (set! now-ms (+ now-ms ms))))
             (cons 'chain-id "timeout-chain")
             (cons 'config-dir TMP-DIR)
             (cons 'emit (lambda (ev . args) (set! events (cons (cons ev args) events))))
             (cons 'step-ms 200)
           ) ;list
         ) ;pi-recovery-run
       ) ;resp
       (log-file (resolve-alarm-log-path TMP-DIR))
      ) ;
  (check (> attempts 1) => #t)
  (check (pi-assistant-message-stop-reason resp) => "error")
  (check-true (string? (pi-assistant-message-error-message resp)))
  (check (list-ref (assoc 'recovery_end events) 2) => 'timeout)
  (check-true (file-exists? log-file))
  (let ((content (path-read-text log-file)))
    (check-true (string-contains? content "[EXHAUSTED]"))
    (check-true (string-contains? content "timeout-chain"))
    (check-true (string-contains? content "wait-timeout"))
  ) ;let
) ;let*

;; 6. retryIntervalMs 非法（0）时回退默认 60s，不变成无限等待或立即超时：
;;    等待目标被 maxWaitMs 剩余预算截断，仍在预算耗尽后返回超时错误
(let* ((cfg (make-all-in-one-cfg #t 2000 0 8000 '()))
       (now-ms 1000)
       (attempts 0)
       (resp
         (pi-recovery-run (lambda (steering) (set! attempts (+ attempts 1)) 'all-nodes-exhausted)
           cfg
           (list (cons 'now-fn (lambda () now-ms))
             (cons 'sleep-fn (lambda (ms) (set! now-ms (+ now-ms ms))))
             (cons 'chain-id "retry-zero-chain")
             (cons 'config-dir TMP-DIR)
             (cons 'step-ms 200)
           ) ;list
         ) ;pi-recovery-run
       ) ;resp
      ) ;
  (check attempts => 1)
  (check (pi-assistant-message-stop-reason resp) => "error")
  ;; 时钟只推进到预算上限，未按 60s 默认间隔无限等待
  (check (- now-ms 1000) => 2000)
) ;let*

;; 7. 墙钟回拨不影响预算：剩余时间全部由注入的单调时钟计算
(let* ((cfg (make-all-in-one-cfg #t 1000 500 8000 '()))
       (now-ms 5000)
       (attempts 0)
       (resp
         (pi-recovery-run (lambda (steering) (set! attempts (+ attempts 1)) 'all-nodes-exhausted)
           cfg
           (list (cons 'now-fn (lambda () now-ms))
             ;; 模拟系统时间回拨：时钟倒退，单调时钟注入值仍只前进
             (cons 'sleep-fn (lambda (ms) (set! now-ms (+ now-ms ms))))
             (cons 'chain-id "monotonic-chain")
             (cons 'config-dir TMP-DIR)
             (cons 'step-ms 100)
           ) ;list
         ) ;pi-recovery-run
       ) ;resp
      ) ;
  (check (pi-assistant-message-stop-reason resp) => "error")
  (check-true (>= attempts 1))
) ;let*

(check-report)
