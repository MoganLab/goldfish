;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; all-in-one 全链故障长程守护与原请求重试引擎：
;; 1. 全链节点耗尽时，若 continuous 开启则进入可中断的步进式等待；
;; 2. maxWaitMs 作为单次连续故障 episode 的累计时间预算，超时记录 [EXHAUSTED] 日志；
;; 3. 短步进休眠期间持续检测 Esc 中止与用户直接输入的新消息；
;; 4. 收到新消息立即结束等待，交回前台作为新任务（不再盲目重试）；
;; 5. 到期重试成功后用原上下文继续当前 Agent 任务；
;; 5. 纯 Scheme 调度循环，支持注入单调时钟、休眠与取消探测钩子。
(define-library (liii pi code all-in-one recovery)
  (import (scheme base)
    (liii date-time)
    (liii time)
    (liii string)
    (liii list)
    (liii pi ai message)
    (liii pi ai abort)
    (liii pi code all-in-one types)
    (liii pi code all-in-one alarm)
  ) ;import
  (export pi-recovery-run)
  (begin

    (define (%opt-ref options key default-val)
      (if (not (pair? options))
        default-val
        (let ((p (assoc key options)))
          (if p (cdr p) default-val)
        ) ;let
      ) ;if
    ) ;define

    (define (%exhausted-message chain-id wait-sec)
      (pi-assistant-message '()
        "error"
        (if (and (number? wait-sec) (> wait-sec 0))
          (string-append "All nodes in chain \""
            chain-id
            "\" failed probe after waiting "
            (number->string wait-sec)
            "s"
          ) ;string-append
          "All nodes in the all-in-one chain failed probe"
        ) ;if
      ) ;pi-assistant-message
    ) ;define

    (define (%aborted-message)
      (pi-assistant-message '() "aborted" "Request aborted by user")
    ) ;define

    ;; 用户直接输入新消息而中止等待：本轮任务就地结束，新消息由前台接手
    (define (%interrupted-message)
      (pi-assistant-message '() "aborted" "Wait aborted by a new user message")
    ) ;define

    ;; 一次到期重试：返回实际响应，或仍全链枯竭时返回 'all-nodes-exhausted
    (define (%attempt! attempt-fn emit chain-id)
      (emit 'recovery_retry chain-id)
      (attempt-fn)
    ) ;define

    ;; 单次等待预算内的步进循环：
    ;; 优先级为 Esc 中止 > 用户新消息唤醒 > 到期重试。
    ;; 用户新消息到达时不再盲目重试（重试仍可能枯竭并重新进入等待），
    ;; 而是返回 'interrupted，由调用方结束本轮 Agent 任务，前台把该消息
    ;; 作为新任务处理，从而实现「输入即可中止等待并继续对话」。
    (define (%step-until-retry! ctx)
      (let* ((now-fn (%opt-ref ctx 'now-fn #f))
             (sleep-fn (%opt-ref ctx 'sleep-fn #f))
             (aborted?-fn (%opt-ref ctx 'aborted? #f))
             (poll-steering (%opt-ref ctx 'poll-steering #f))
             (emit (%opt-ref ctx 'emit #f))
             (chain-id (%opt-ref ctx 'chain-id "all-in-one"))
             (attempt-fn (%opt-ref ctx 'attempt! #f))
             (wait-start (%opt-ref ctx 'wait-start 0))
             (wait-target-ms (%opt-ref ctx 'wait-target-ms 0))
             (step-ms (%opt-ref ctx 'step-ms 100))
            ) ;
        (let loop
          ((last-tick-sec -1))
          (cond ((aborted?-fn)
                 (emit 'recovery_end chain-id 'aborted)
                 (cons (%aborted-message) #f)
                ) ;
                (else
                  (let* ((injected (poll-steering))
                         (user-msgs (if (pair? injected) injected '()))
                         (step-elapsed (- (now-fn) wait-start))
                         (rem-in-wait (- wait-target-ms step-elapsed))
                        ) ;
                    (cond
                      ;; 用户直接输入新消息：立即结束等待，交回前台作为新任务
                      ((pair? user-msgs)
                       (emit 'recovery_end chain-id 'interrupted)
                       (cons (%interrupted-message) user-msgs)
                      ) ;
                      ;; 到期：触发一次重试
                      ((<= rem-in-wait 0) (cons (%attempt! attempt-fn emit chain-id) #f))
                      (else
                        (let* ((curr-sec (quotient (+ rem-in-wait 999) 1000))
                               (to-sleep (min step-ms rem-in-wait))
                              ) ;
                          (when (not (eqv? curr-sec last-tick-sec))
                            (emit 'recovery_tick chain-id rem-in-wait)
                          ) ;when
                          (sleep-fn to-sleep)
                          (loop curr-sec)
                        ) ;let*
                      ) ;else
                    ) ;cond
                  ) ;let*
                ) ;else
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    ;; 预算耗尽：写带 wait-timeout 的 [EXHAUSTED]，并返回可读错误消息
    (define (%wait-timeout! ctx)
      (let ((chain-id (%opt-ref ctx 'chain-id "all-in-one"))
            (config-dir (%opt-ref ctx 'config-dir #f))
            (emit (%opt-ref ctx 'emit #f))
            (max-wait-ms (%opt-ref ctx 'max-wait-ms 0))
           ) ;
        (pi-alarm-log! "EXHAUSTED"
          "all"
          chain-id
          "wait-timeout"
          (string-append "Wait timeout reached (" (number->string max-wait-ms) "ms)")
          config-dir
        ) ;pi-alarm-log!
        (emit 'recovery_end chain-id 'timeout)
        (%exhausted-message chain-id (quotient max-wait-ms 1000))
      ) ;let
    ) ;define

    ;; 一次等待结果的分派：
    ;; - aborted / interrupted：就地结束，交回上层结算
    ;; - 恢复成功：返回真实响应
    ;; - 仍枯竭：进入下一轮等待
    (define (%after-step! ctx resp loop)
      (let ((chain-id (%opt-ref ctx 'chain-id "all-in-one"))
            (emit (%opt-ref ctx 'emit #f))
           ) ;
        (cond
         ((and (pi-assistant-message? resp)
            (equal? (pi-assistant-message-stop-reason resp) "aborted")
          ) ;and
          ;; 中止事件已由 %step-until-retry! 发出
          resp
         ) ;
         ((not (eq? resp 'all-nodes-exhausted))
          (emit 'recovery_end chain-id 'success)
          resp
         ) ;
         (else (loop))
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-recovery-run attempt-fn aio-cfg options)
      (unless (procedure? attempt-fn)
        (error 'type-error "pi-recovery-run: attempt-fn must be a procedure" attempt-fn)
      ) ;unless
      (unless (all-in-one-cfg? aio-cfg)
        (error 'type-error
          "pi-recovery-run: aio-cfg must be an <all-in-one-cfg>"
          aio-cfg
        ) ;error
      ) ;unless
      (if (not (all-in-one-cfg-continuous? aio-cfg))
        (%exhausted-message "all-in-one" #f)
        (let* ((chain-id (%opt-ref options 'chain-id "all-in-one"))
               (now-fn (%opt-ref options 'now-fn current-monotonic-millis))
               (max-wait-ms (max 1 (all-in-one-cfg-max-wait-ms aio-cfg)))
               ;; retryIntervalMs 非法（0 或负）时回退 loader 默认 60s，避免立即超时
               (retry-interval-ms
                 (let ((r (all-in-one-cfg-retry-interval-ms aio-cfg)))
                   (if (and (number? r) (> r 0)) r 60000)
                 ) ;let
               ) ;retry-interval-ms
               (emit (%opt-ref options 'emit (lambda (type . args) #f)))
               (start-ms (now-fn))
               (ctx
                 (list (cons 'now-fn now-fn)
                   (cons 'sleep-fn
                     (%opt-ref options 'sleep-fn (lambda (ms) (sleep (/ ms 1000.0))))
                   ) ;cons
                   (cons 'aborted? (%opt-ref options 'aborted? pi-aborted?))
                   (cons 'poll-steering (%opt-ref options 'poll-steering (lambda () '())))
                   (cons 'emit emit)
                   (cons 'chain-id chain-id)
                   (cons 'config-dir (%opt-ref options 'config-dir #f))
                   (cons 'max-wait-ms max-wait-ms)
                   (cons 'attempt! attempt-fn)
                   (cons 'step-ms (%opt-ref options 'step-ms 100))
                 ) ;list
               ) ;ctx
              ) ;
          (emit 'recovery_wait chain-id max-wait-ms max-wait-ms)
          ;; 跨重试累计消耗全局 maxWaitMs 预算
          (let loop
            ()
            (let* ((remaining-budget (- max-wait-ms (- (now-fn) start-ms)))
                   (wait-target-ms (min retry-interval-ms remaining-budget))
                  ) ;
              (if (<= wait-target-ms 0)
                (%wait-timeout! ctx)
                (let* ((ctx* (%ctx-with-wait ctx wait-target-ms)) (result (%step-until-retry! ctx*)))
                  (%after-step! ctx (car result) loop)
                ) ;let*
              ) ;if
            ) ;let*
          ) ;let
        ) ;let*
      ) ;if
    ) ;define

    ;; 为单次等待补上起点与等待目标
    (define (%ctx-with-wait ctx wait-target-ms)
      (cons (cons 'wait-start ((%opt-ref ctx 'now-fn current-monotonic-millis)))
        (cons (cons 'wait-target-ms wait-target-ms) ctx)
      ) ;cons
    ) ;define

  ) ;begin
) ;define-library
