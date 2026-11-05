;; run-agent-loop-with-timeout
;; 以解析出的超时与轮数上限驱动 Agent 主循环。
;;
;; 语法
;; ----
;; (run-agent-loop-with-timeout cfg init-msgs system-prompt emit parsed compaction emit-initial?)
;;
;; 参数
;; ----
;; cfg : alist
;; 运行配置 alist，含 tools、provider 与 model-id。
;; init-msgs : list
;; 初始消息列表。
;; system-prompt : string | procedure
;; 系统提示词文本或回调。
;; emit : procedure | #f
;; 事件派发回调。
;; parsed : alist
;; 解析后的命令行参数 alist；session-timeout 为负数时立即以超时错误结算。
;; compaction : list | #f
;; 上下文压缩配置。
;; emit-initial? : boolean
;; 是否派发初始消息的上屏事件。
;;
;; 返回值
;; ----
;; list
;; 执行完成后的完整消息历史列表。

(import (scheme base)
  (liii check)
  (liii pi ai message)
  (liii pi code modes flow)
) ;import

(check-set-mode! 'report-failed)

(define CFG
  (list (cons 'tools '())
    (cons 'provider "google")
    (cons 'model-id "gemini-3.8-flash")
  ) ;list
) ;define

;; 统计指定类型事件的派发次数
(define (count-events evs type)
  (let loop
    ((rest evs) (n 0))
    (if (null? rest)
      n
      (loop (cdr rest) (if (eq? (car rest) type) (+ n 1) n))
    ) ;loop
  ) ;let
) ;define

;; 1. 会话超时为负数：立即以超时错误结算，不发起模型请求
(let* ((parsed (list (cons 'session-timeout -1)))
       (res (run-agent-loop-with-timeout CFG (list (pi-user-message "hi")) "" #f parsed))
      ) ;
  (check (length res) => 2)
  (check-true (pi-user-message? (car res)))
  (check-true (pi-assistant-message? (cadr res)))
  (check (pi-assistant-message-stop-reason (cadr res)) => "error")
  (check (pi-assistant-message-error-message (cadr res))
    => "Session timeout exceeded: total execution deadline reached"))

;; 2. 事件派发：初始消息上屏与错误结算事件均已派发
(let* ((evs '())
       (emit (lambda (ev-type . args) (set! evs (cons ev-type evs))))
       (parsed (list (cons 'session-timeout -1)))
       (res (run-agent-loop-with-timeout CFG (list (pi-user-message "hi")) "" emit parsed))
      ) ;
  (check (length res) => 2)
  (check-true (pair? (member 'agent_start evs)))
  (check-true (pair? (member 'message_start evs)))
  (check-true (pair? (member 'message_end evs)))
  (check-true (pair? (member 'agent_end evs))))

;; 3. emit-initial? 为 #f 时不派发初始消息的上屏事件（仅剩错误消息一条）
(let* ((evs '())
       (emit (lambda (ev-type . args) (set! evs (cons ev-type evs))))
       (parsed (list (cons 'session-timeout -1)))
       (res (run-agent-loop-with-timeout CFG (list (pi-user-message "hi")) "" emit parsed #f #f))
       (msg-starts (count-events evs 'message_start))
      ) ;
  (check (length res) => 2)
  (check msg-starts => 1))

(check-report)
