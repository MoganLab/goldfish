(import (scheme base)
  (liii check)
  (liii list)
  (liii go)
  (liii string)
  (liii pi agent loop)
  (liii pi ai message)
  (liii pi code cli args)
  (liii pi code modes interactive worker)
) ;import

(check-set-mode! 'report-failed)

;; -----------------------------------------------------------------------------
;; 回归：worker 任务 cfg 必须注入 steering / follow-up 轮询回调
;;
;; 缺陷现象：TUI 中 busy 时回车（steering）提交的消息永远不会被模型处理，
;; 只有空闲时提交的第一条消息真正生效（消息停留在通道里，既不进入模型
;; 上下文，也不驱动新一轮请求）。
;;
;; 根因：worker 侧构造了带 get-steering-messages / get-follow-up-messages
;; 回调的运行期 cfg，却把它丢弃，agent 任务用的 cfg 里没有这两个回调，
;; run-agent-loop 的轮询点永远取不到消息。
;;
;; 本测试覆盖：
;;   1. pi-worker-handle-job 返回的任务 cfg 含两个轮询回调，且回调能真正
;;      从通道取消息并回发 user_message 事件；
;;   2. 用该 cfg 驱动 run-agent-loop：运行中投递的多条 steering 消息
;;      最终全部进入模型上下文（对应“1/2/3/4 只有 1 被处理”的场景）。
;; -----------------------------------------------------------------------------

(define (make-text-response text)
  (pi-assistant-message
    (list (list (cons 'type "text") (cons 'text text)))
    :stop-reason "stop"
    :usage '((input . 1) (output . 1) (total . 2))
  ) ;pi-assistant-message
) ;define

(define (user-texts-of msgs)
  (map pi-user-message-content
    (filter pi-user-message? msgs)
  ) ;map
) ;define

;; worker 任务 cfg（经生产路径 pi-worker-handle-job 取得）：
;; bash_record 任务不触发 LLM 请求，用 --no-session 避免任何落盘副作用
(define (make-worker-runtime-cfg events steer fu)
  (pi-worker-handle-job
    (list 'bash_record "echo hi" "hi" 0)
    (parse-pi-args '("--no-session"))
    '((provider . "test") (tools . ()))
    (vector #f)
    events
    #f
    steer
    fu
  ) ;pi-worker-handle-job
) ;define

;; -----------------------------------------------------------------------------
;; 测试项 1: 任务 cfg 注入轮询回调，回调从通道取消息并回发 user_message 事件
;; -----------------------------------------------------------------------------
(let* ((events (make-chan 16))
       (steer (make-chan 16))
       (fu (make-chan 16))
       (runtime-cfg (make-worker-runtime-cfg events steer fu))
      ) ;
  (check-true (pair? (assoc 'get-steering-messages runtime-cfg)))
  (check-true (pair? (assoc 'get-follow-up-messages runtime-cfg)))

  ;; steering 回调：取到消息并回发事件
  (chan-send! steer "插话" 1000)
  (let ((poll (cdr (assoc 'get-steering-messages runtime-cfg))))
    (check (poll) => '("插话"))
    (check (chan-try-recv! events #f) => '(user_message "插话"))
    ;; 通道空时返回空表（不抛错）
    (check (poll) => '())
  ) ;let

  ;; follow-up 回调：同上
  (chan-send! fu "排队" 1000)
  (let ((poll (cdr (assoc 'get-follow-up-messages runtime-cfg))))
    (check (poll) => '("排队"))
    (check (chan-try-recv! events #f) => '(user_message "排队"))
    (check (poll) => '())
  ) ;let
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 2: 多轮任务后 cfg 不累积重复回调项（同名键替换而非追加）
;; -----------------------------------------------------------------------------
(let* ((events (make-chan 16))
       (steer (make-chan 16))
       (fu (make-chan 16))
       (cfg1 (make-worker-runtime-cfg events steer fu))
       (cfg2 (make-worker-runtime-cfg events steer fu))
      ) ;
  (check (length (filter (lambda (p) (eq? (car p) 'get-steering-messages)) cfg2)) => 1)
  (check (length (filter (lambda (p) (eq? (car p) 'get-follow-up-messages)) cfg2)) => 1)
  ;; 原有 cfg 项不受影响
  (check (cdr (assoc 'provider cfg2)) => "test")
  (check-true (pair? cfg1))
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 3: 运行中投递的多条 steering 消息最终全部进入模型上下文
;;   （复现“连续输入 1/2/3/4，只有 1 被处理”的场景）
;; -----------------------------------------------------------------------------
(let* ((events (make-chan 16))
       (steer (make-chan 16))
       (fu (make-chan 16))
       (runtime-cfg (make-worker-runtime-cfg events steer fu))
       (req-count (vector 0))
       (req-users (vector '()))
       (request-fn
         (lambda (provider config msgs sys-prompt tools on-ev timeout)
           (vector-set! req-count 0 (+ 1 (vector-ref req-count 0)))
           (vector-set! req-users 0
             (append (vector-ref req-users 0)
               (list (user-texts-of msgs))
             ) ;list
           ) ;vector-set!
           ;; 第 1 轮请求发出后模拟用户继续输入（steering 插话）
           (when (= (vector-ref req-count 0) 1)
             (chan-send! steer "第二条" 1000)
             (chan-send! steer "第三条" 1000)
           ) ;when
           (make-text-response "ok")
         ) ;lambda
        )
      ) ;
  (let ((res-msgs (run-agent-loop runtime-cfg
                   (list (pi-user-message "第一条"))
                   "sys"
                   #f
                   10
                   5
                   600
                   #f
                   request-fn
                 ) ;run-agent-loop
               ) ;res-msgs
         ) ;
    ;; 三条消息全部进入模型上下文（此前只有第一条能进）
    (check (user-texts-of res-msgs) => '("第一条" "第二条" "第三条"))
    ;; 最后一次请求的上下文包含全部三条
    (check (car (reverse (vector-ref req-users 0)))
      => '("第一条" "第二条" "第三条")
    ) ;check
    ;; 两条插话都回发了 user_message 事件（主线程据此上屏用户卡片）
    (check (chan-try-recv! events #f) => '(user_message "第二条"))
    (check (chan-try-recv! events #f) => '(user_message "第三条"))
    (check (chan-try-recv! events #f) => #f)
    ;; steering 通道已排空
    (check (chan-try-recv! steer #f) => #f)
  ) ;let
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 4: follow-up 在自然停止时注入并驱动新一轮
;; -----------------------------------------------------------------------------
(let* ((events (make-chan 16))
       (steer (make-chan 16))
       (fu (make-chan 16))
       (runtime-cfg (make-worker-runtime-cfg events steer fu))
       (req-count (vector 0))
       (request-fn
         (lambda (provider config msgs sys-prompt tools on-ev timeout)
           (vector-set! req-count 0 (+ 1 (vector-ref req-count 0)))
           (make-text-response "ok")
         ) ;lambda
        )
      ) ;
  (chan-send! fu "结束后总结" 1000)
  (let ((res-msgs (run-agent-loop runtime-cfg
                   (list (pi-user-message "第一条"))
                   "sys"
                   #f
                   10
                   5
                   600
                   #f
                   request-fn
                 ) ;run-agent-loop
               ) ;res-msgs
         ) ;
    (check (vector-ref req-count 0) => 2)
    (check (user-texts-of res-msgs) => '("第一条" "结束后总结"))
    (check (chan-try-recv! events #f) => '(user_message "结束后总结"))
  ) ;let
) ;let*

(check-report)
