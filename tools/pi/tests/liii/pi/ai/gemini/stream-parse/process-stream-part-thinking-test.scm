(import (liii check)
        (liii pi ai message)
        (liii pi ai gemini stream-parse)
        (liii pi ai gemini thinking))

(check-set-mode! 'report-failed)

;; 测试 process-stream-part 在流式接收 thinking 块并过渡到普通 text 块时的事件和结构
(let* ((stream-state (vector '() #f '((input . 10) (output . 5)) '() #f #f #f #f))
       (events '())
       (emit (lambda (ev) (set! events (append events (list ev))))))

  ;; 1. 第一个 chunk: thinking 开始
  (process-stream-part '(("text" . "Let me") ("thought" . #t) ("thoughtSignature" . "sig1"))
                       stream-state
                       emit)
  (check (length (vector-ref stream-state 0)) => 1)
  (check (pi-thinking-block? (car (vector-ref stream-state 0))) => #t)
  (check (length events) => 2)
  (check (cdr (assoc 'type (car events))) => "thinking_start")
  (check (cdr (assoc 'type (cadr events))) => "thinking_delta")

  ;; 2. 第二个 chunk: thinking 追加增量
  (process-stream-part '(("text" . " analyze this") ("thought" . #t))
                       stream-state
                       emit)
  (check (length (vector-ref stream-state 0)) => 1)
  (check (length events) => 3)
  (check (cdr (assoc 'type (list-ref events 2))) => "thinking_delta")
  (check (pi-thinking-block-thinking (car (vector-ref stream-state 0))) => "Let me analyze this")

  ;; 3. 第三个 chunk: 思考结束，进入普通回答 text
  (process-stream-part '(("text" . "Here is the result."))
                       stream-state
                       emit)
  (check (length (vector-ref stream-state 0)) => 2)
  (check (pi-thinking-block? (car (vector-ref stream-state 0))) => #t)
  (check (pi-text-block? (cadr (vector-ref stream-state 0))) => #t)
  ;; 应该先触发 thinking_end，再触发 text_start, text_delta
  (check (length events) => 6)
  (check (cdr (assoc 'type (list-ref events 3))) => "thinking_end")
  (check (cdr (assoc 'type (list-ref events 4))) => "text_start")
  (check (cdr (assoc 'type (list-ref events 5))) => "text_delta")

  ;; 4. 流正常结束时，调用 stop-active-block
  (gemini-stream-stop-active-block! stream-state emit)
  (check (length events) => 7)
  (check (cdr (assoc 'type (list-ref events 6))) => "text_end")
  (check (cdr (assoc 'content (list-ref events 6))) => "Here is the result."))

(check-report)
