;; process-sse-chunk-data
;; 处理 Gemini 流式响应的一个 SSE 数据块，更新用量、停止原因与内容块。
;;
;; 语法
;; ----
;; (process-sse-chunk-data json-obj stream-state on-event)
;;
;; 参数
;; ----
;; json-obj : alist
;; 已解析的 Gemini SSE 数据对象，包含 candidates 与 usageMetadata。
;; stream-state : gemini-stream-state | vector
;; 可变流状态；record 为当前表示，vector 用于兼容旧调用。
;; on-event : procedure | #f
;; 接收流式事件的回调；为 #f 时不派发事件。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用更新流状态并派发内容块事件。
;;
;; 说明
;; ----
;; 只处理第一个 candidate；usageMetadata 存在时覆盖当前用量统计。

(import (liii check)
  (liii pi ai message)
  (liii pi ai api google-generative-ai stream-parse)
  (liii pi ai api google-generative-ai stream-state)
) ;import

(check-set-mode! 'report-failed)

;; 文本 part 与 finishReason
(let* ((stream-state (vector '() #f '() '() "STOP" #f "" ""))
       (events '())
       (emit (lambda (ev) (set! events (append events (list ev)))))
       (part (list (cons "text" "Hi")))
       (candidate
         (list (cons "content" (list (cons "parts" (vector part))))
               (cons "finishReason" "STOP")))
       (usage-meta
         (list (cons "promptTokenCount" 10)
               (cons "candidatesTokenCount" 4)
               (cons "totalTokenCount" 14)))
       (json-obj
         (list (cons "candidates" (vector candidate))
               (cons "usageMetadata" usage-meta)))
      ) ;
  (process-sse-chunk-data json-obj stream-state emit)
  (check (gemini-stream-state-finish-reason stream-state) => "STOP")
  (check (length (gemini-stream-state-blocks stream-state)) => 1)
  (check (pi-text-block-text (car (gemini-stream-state-blocks stream-state))) => "Hi")
  (check (cdr (assoc 'input (gemini-stream-state-usage stream-state))) => 10)
  (check (cdr (assoc 'output (gemini-stream-state-usage stream-state))) => 4)
  (check (length events) => 2)
) ;let*

;; functionCall part
(let* ((stream-state (vector '() #f '() '() "STOP" #f "" ""))
       (fc (list (cons "name" "bash")
                 (cons "args" (list (cons "command" "ls")))
                 (cons "id" "call_1")))
       (part (list (cons "functionCall" fc)))
       (candidate (list (cons "content" (list (cons "parts" (vector part))))))
       (json-obj (list (cons "candidates" (vector candidate))))
      ) ;
  (process-sse-chunk-data json-obj stream-state #f)
  (check (length (gemini-stream-state-blocks stream-state)) => 1)
  (check (pi-tool-call-block-name (car (gemini-stream-state-blocks stream-state))) => "bash")
) ;let*

;; 空 candidates 被忽略
(let ((stream-state (vector '() #f '() '() "STOP" #f "" "")))
  (process-sse-chunk-data (list (cons "candidates" (vector))) stream-state #f)
  (check (length (gemini-stream-state-blocks stream-state)) => 0)
) ;let*

(check-report)
