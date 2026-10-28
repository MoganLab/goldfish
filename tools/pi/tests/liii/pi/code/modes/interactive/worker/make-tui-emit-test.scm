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

;; 模拟 ESC 中止后再发消息触发 HTTP 400 的错误文本
(define ERR-TEXT
  "HTTP 400: the message at position 2 with role 'assistant' must not be empty")

;; 非阻塞收取下一条事件；通道空时返回哨兵 'none
(define (recv-ev! ch)
  (chan-try-recv! ch 'none)
) ;define

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
