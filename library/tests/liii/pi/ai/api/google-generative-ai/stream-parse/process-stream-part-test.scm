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

(import (liii check)
  (liii pi ai message)
  (liii pi ai api google-generative-ai stream-parse)
  (liii pi ai api google-generative-ai stream-state)
  (liii pi ai api google-generative-ai thinking)
) ;import

(check-set-mode! 'report-failed)

;; 普通文本 part
(let* ((stream-state (vector '() #f '() '() "STOP" #f "" ""))
       (events '())
       (emit (lambda (ev) (set! events (append events (list ev)))))
      ) ;
  (process-stream-part '(("text" . "Hello")) stream-state emit)
  (check (length (gemini-stream-state-blocks stream-state)) => 1)
  (check (pi-text-block-text (car (gemini-stream-state-blocks stream-state))) => "Hello")
  (check (gemini-stream-state-current-active-type stream-state) => "text")
  (check (length events) => 2)
  (check (cdr (assoc 'type (car events))) => "text_start")
  (check (cdr (assoc 'type (cadr events))) => "text_delta")
) ;let*

;; functionCall part
(let* ((stream-state (vector '() #f '() '() "STOP" #f "" ""))
       (events '())
       (emit (lambda (ev) (set! events (append events (list ev)))))
      ) ;
  (process-stream-part '(("functionCall" . (("name" . "bash")
                                            ("args" . (("command" . "ls")))
                                            ("id" . "call_1"))))
    stream-state
    emit
  ) ;process-stream-part
  (check (length (gemini-stream-state-blocks stream-state)) => 1)
  (check (pi-tool-call-block-name (car (gemini-stream-state-blocks stream-state))) => "bash")
  (check (length events) => 3)
  (check (cdr (assoc 'type (car events))) => "toolcall_start")
) ;let*

;; 未知 part 被忽略
(let* ((stream-state (vector '() #f '() '() "STOP" #f "" ""))
       (events '())
       (emit (lambda (ev) (set! events (append events (list ev)))))
      ) ;
  (process-stream-part '(("unknown" . "x")) stream-state emit)
  (check (length (gemini-stream-state-blocks stream-state)) => 0)
  (check (length events) => 0)
) ;let*

(check-report)

;; 文本块签名累积与回放
(let* ((stream-state (make-gemini-stream-state '() #f '() '() "STOP" #f "" ""))
       (emit (lambda (ev) #t))
      ) ;
  ;; 第一个 text part 带签名，第二个 text part 不带签名（同一块累积）
  (process-stream-part '(("text" . "Hello") ("thoughtSignature" . "QUJD")) stream-state emit)
  (process-stream-part '(("text" . " world")) stream-state emit)
  (let ((blk (car (gemini-stream-state-blocks stream-state))))
    (check (pi-text-block-text blk) => "Hello world")
    (check (pi-text-block-signature blk) => "QUJD")
  ) ;let
) ;let*

;; 后续 part 带新签名时覆盖（retainThoughtSignature 语义）
(let* ((stream-state (make-gemini-stream-state '() #f '() '() "STOP" #f "" ""))
       (emit (lambda (ev) #t))
      ) ;
  (process-stream-part '(("text" . "a") ("thoughtSignature" . "QUJD")) stream-state emit)
  (process-stream-part '(("text" . "b") ("thoughtSignature" . "REVG")) stream-state emit)
  (let ((blk (car (gemini-stream-state-blocks stream-state))))
    (check (pi-text-block-text blk) => "ab")
    (check (pi-text-block-signature blk) => "REVG")
  ) ;let
) ;let*

;; 无签名文本块：签名为 #f
(let* ((stream-state (make-gemini-stream-state '() #f '() '() "STOP" #f "" ""))
       (emit (lambda (ev) #t))
      ) ;
  (process-stream-part '(("text" . "plain")) stream-state emit)
  (check (pi-text-block-signature (car (gemini-stream-state-blocks stream-state))) => #f)
) ;let*

(check-report)
