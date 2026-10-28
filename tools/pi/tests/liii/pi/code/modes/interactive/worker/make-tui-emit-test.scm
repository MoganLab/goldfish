(import (scheme base)
        (scheme write)
        (liii check)
        (liii string)
        (liii go)
        (liii tui)
        (liii pi ai message)
        (liii pi agent loop)
        (liii pi code modes interactive)
        (liii pi code modes interactive worker))

(check-set-mode! 'report-failed)

;; make-tui-emit
;; 将 agent 会话事件翻译为 TUI 卡片事件并投递到事件通道（生产与测试共用）。
;;
;; 语法
;; ----
;; (make-tui-emit event-chan)
;;
;; 参数
;; ----
;; event-chan : chan?
;; 事件通道，TUI 主线程通过 pi-app-poll-events! 从该通道轮询卡片事件。
;;
;; 返回值
;; ----
;; procedure?
;; emit 回调函数，接受 (event-type . args) 形式的可变参数，可直接传给
;; run-agent-loop 的 emit 参数。
;;
;; 描述
;; ----
;; 该回调将 agent loop 的会话事件翻译为 TUI 可消费的卡片事件：
;; 1. message_end：assistant 消息文本非空时投递 (assistant_text text)；
;;    stopReason=error 时投递 (error errorMessage)，errorMessage 缺省时
;;    兜底为 "Error"（对齐上游 errorMessage || "Error" 语义）；
;; 2. tool_execution_start/update/end：工具卡片的原地推进事件；
;; 3. retry：重试提示卡片事件；
;; 4. agent_settled：携带 aborted? 标志，主线程据此上屏 Aborted 提示。
;; 其余事件类型（agent_start/turn_start/message_start 等）被忽略。

;; 模拟 ESC 中止后再发消息触发 HTTP 400 的错误文本
(define ERR-TEXT
  "HTTP 400: the message at position 2 with role 'assistant' must not be empty")

;; 非阻塞收取下一条事件；通道空时返回哨兵 'none
(define (recv-ev! ch)
  (chan-try-recv! ch 'none)
) ;define

;; 0. %assistant-error-text：error 结算提取错误文本，缺省兜底 "Error"，非 error 返回 #f
(check (%assistant-error-text
          (pi-assistant-message '() :stop-reason "error" :error-message "boom"))
  => "boom")
(check (%assistant-error-text (pi-assistant-message '() :stop-reason "error")) => "Error")
(check (%assistant-error-text (pi-assistant-message '() :stop-reason "aborted")) => #f)

;; 1. error 结算且无部分文本：仅投递 (error errorMessage)，不投递 assistant_text
(let* ((ch (make-chan 16))
       (emit (make-tui-emit ch))
       (msg (pi-assistant-message '() :stop-reason "error" :error-message ERR-TEXT)))
  (emit 'message_end msg)
  (check (recv-ev! ch) => (list 'error ERR-TEXT))
  (check (recv-ev! ch) => 'none))

;; 2. error 结算带部分文本：先 assistant_text 后 error
(let* ((ch (make-chan 16))
       (emit (make-tui-emit ch))
       (msg (pi-assistant-message (list (pi-text-block "partial"))
              :stop-reason "error" :error-message ERR-TEXT)))
  (emit 'message_end msg)
  (check (recv-ev! ch) => (list 'assistant_text "partial"))
  (check (recv-ev! ch) => (list 'error ERR-TEXT))
  (check (recv-ev! ch) => 'none))

;; 3. 正常 stop 结算：仅 assistant_text，无 error 事件
(let* ((ch (make-chan 16))
       (emit (make-tui-emit ch))
       (msg (pi-assistant-message (list (pi-text-block "ok")))))
  (emit 'message_end msg)
  (check (recv-ev! ch) => (list 'assistant_text "ok"))
  (check (recv-ev! ch) => 'none))

;; 4. aborted 空消息（本 bug 复现场景）：不投递任何事件，保持现有行为
(let* ((ch (make-chan 16))
       (emit (make-tui-emit ch))
       (msg (pi-assistant-message '() :stop-reason "aborted")))
  (emit 'message_end msg)
  (check (recv-ev! ch) => 'none))

;; 5. error 结算但 errorMessage 缺省：兜底投递 (error "Error")
(let* ((ch (make-chan 16))
       (emit (make-tui-emit ch))
       (msg (pi-assistant-message '() :stop-reason "error")))
  (emit 'message_end msg)
  (check (recv-ev! ch) => (list 'error "Error"))
  (check (recv-ev! ch) => 'none))

;; 6. E2E：run-agent-loop 收到 error 结算，UI 出现错误卡片并渲染错误文本
(let* ((app (make-pi-app :terminal (make-virtual-terminal 100 40)
              :handler (lambda (a m) #t)))
       (emit (make-tui-emit (pi-app-event-chan app)))
       (cfg '((provider . "test") (tools . ()) (settings . ())))
       (err-msg (pi-assistant-message '() :stop-reason "error" :error-message ERR-TEXT))
       (request-fn
         (lambda (provider config msgs sys-prompt tools on-ev timeout) err-msg)))
  (run-agent-loop cfg '() "sys" emit 10 5 600 #f request-fn #f)
  (pi-app-poll-events! app)
  ;; chat 中恰有一张 error 卡片，屏幕可见 HTTP 400 错误文本
  (check (length (pi-app-chat app)) => 1)
  (let ((text (string-join (map strip-ansi (screen-lines (pi-app-screen app))) "\n")))
    (check-true (string-contains? text "HTTP 400"))))

(check-report)
