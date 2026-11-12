;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; all-in-one 全链故障长程守护与原请求重试引擎：
;; 1. 全链节点耗尽时，若 continuous 开启则进入可中断的步进式等待；
;; 2. maxWaitMs 作为单次连续故障 episode 的累计时间预算，超时记录 [EXHAUSTED] 日志；
;; 3. 短步进休眠期间持续检测 Esc 中断与 Enter steering 消息注入；
;; 4. 收到 steering 消息立即唤醒重试，成功后携带原上下文继续主循环；
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

    ;; 一次唤醒重试：返回实际响应，或仍全链枯竭时返回 'all-nodes-exhausted
    (define (%attempt! attempt-fn emit chain-id steering)
      (emit 'recovery_retry chain-id)
      (attempt-fn steering)
    ) ;define

    ;; 单次等待预算内的步进循环：abort > steering 唤醒 > 到期重试
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
          ((steering (%opt-ref ctx 'steering '())) (last-tick-sec -1))
          (cond ((aborted?-fn)
                 (emit 'recovery_end chain-id 'aborted)
                 (cons (%aborted-message) steering)
                ) ;
                (else
                  (let* ((injected (poll-steering))
                         (steering-list (if (pair? injected) injected '()))
                         (new-steering (append steering steering-list))
                         (step-elapsed (- (now-fn) wait-start))
                         (rem-in-wait (- wait-target-ms step-elapsed))
                        ) ;
                    (cond
                      ;; steering 消息到达或到期：立即触发一次重试
                      ((or (pair? steering-list) (<= rem-in-wait 0))
                       (cons (%attempt! attempt-fn emit chain-id new-steering) new-steering)
                      ) ;
                      (else
                        (let* ((curr-sec (quotient (+ rem-in-wait 999) 1000))
                               (to-sleep (min step-ms rem-in-wait))
                              ) ;
                          (when (not (eqv? curr-sec last-tick-sec))
                            (emit 'recovery_tick chain-id rem-in-wait)
                          ) ;when
                          (sleep-fn to-sleep)
                          (loop new-steering curr-sec)
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

    ;; 一次唤醒结果的分派：aborted / 恢复成功 / 仍枯竭时进入下一轮等待
    (define (%after-step! ctx resp steering loop)
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
         (else (loop steering))
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
            ((steering '()))
            (let* ((remaining-budget (- max-wait-ms (- (now-fn) start-ms)))
                   (wait-target-ms (min retry-interval-ms remaining-budget))
                  ) ;
              (if (<= wait-target-ms 0)
                (%wait-timeout! ctx)
                (let* ((ctx* (%ctx-with-wait ctx steering wait-target-ms))
                       (result (%step-until-retry! ctx*))
                      ) ;
                  (%after-step! ctx (car result) (cdr result) loop)
                ) ;let*
              ) ;if
            ) ;let*
          ) ;let
        ) ;let*
      ) ;if
    ) ;define

    ;; 为单次等待补上起点、目标与当前累计的 steering
    (define (%ctx-with-wait ctx steering wait-target-ms)
      (cons (cons 'wait-start ((%opt-ref ctx 'now-fn current-monotonic-millis)))
        (cons (cons 'wait-target-ms wait-target-ms)
          (cons (cons 'steering steering) ctx)
        ) ;cons
      ) ;cons
    ) ;define

  ) ;begin
) ;define-library
