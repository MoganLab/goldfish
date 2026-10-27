(import (liii check)
  (liii pi ai message)
  (liii pi ai api google-generative-ai stream-parse)
  (liii pi ai api google-generative-ai thinking)
) ;import

(check-set-mode! 'report-failed)

;; process-stream-part
;; 处理 Gemini 流式响应中的单个 part，并更新文本、thinking 或工具调用状态。
;;
;; 语法
;; ----
;; (process-stream-part part stream-state on-event)
;;
;; 参数
;; ----
;; part : alist
;; 包含 text、thought、thoughtSignature 或 functionCall 的响应片段。
;; stream-state : gemini-stream-state | vector
;; 可变流状态；record 为当前表示，vector 用于兼容旧调用。
;; on-event : procedure | #f
;; 接收流式事件的回调；为 #f 时不派发事件。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用更新内容块、当前活动块和工具调用列表。
;;
;; 说明
;; ----
;; 从 thinking 切换到普通文本时，会先结束 thinking 块再开始文本块。
;;
;; 示例
;; ----
;; (process-stream-part '(("text" . "Hello")) stream-state on-event)

;; 测试 process-stream-part 在流式接收 thinking 块并过渡到普通 text 块时的事件和结构
(let* ((stream-state (vector '() #f '((input . 10) (output . 5)) '() #f #f #f #f))
       (events '())
       (emit
         (lambda (ev) (set! events (append events (list ev))))
       ) ;emit
      ) ;

  ;; 1. 第一个 chunk: thinking 开始
  (process-stream-part '(("text" . "Let me")
                         ("thought" . #t)
                         ("thoughtSignature" . "sig1"))
    stream-state
    emit
  ) ;process-stream-part
  (check (length (vector-ref stream-state 0)) => 1)
  (check (pi-thinking-block? (car (vector-ref stream-state 0))) => #t)
  (check (length events) => 2)
  (check (cdr (assoc 'type (car events))) => "thinking_start")
  (check (cdr (assoc 'type (cadr events))) => "thinking_delta")

  ;; 2. 第二个 chunk: thinking 追加增量
  (process-stream-part '(("text" . " analyze this") ("thought" . #t))
    stream-state
    emit
  ) ;process-stream-part
  (check (length (vector-ref stream-state 0)) => 1)
  (check (length events) => 3)
  (check (cdr (assoc 'type (list-ref events 2))) => "thinking_delta")
  (check (pi-thinking-block-thinking (car (vector-ref stream-state 0)))
    =>
    "Let me analyze this"
  ) ;check

  ;; 3. 第三个 chunk: 思考结束，进入普通回答 text
  (process-stream-part '(("text" . "Here is the result.")) stream-state emit)
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
  (check (cdr (assoc 'content (list-ref events 6))) => "Here is the result.")
) ;let*

(check-report)
