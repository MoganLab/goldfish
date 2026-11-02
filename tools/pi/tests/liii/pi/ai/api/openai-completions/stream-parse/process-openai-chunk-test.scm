;; process-openai-chunk
;; 处理单个 OpenAI Chat Completions chunk JSON 对象，更新用量、停止原因与文本/工具增量。
;;
;; 语法
;; ----
;; (process-openai-chunk json-obj stream-state on-event)
;;
;; 参数
;; ----
;; json-obj : alist
;; 已解析的 chunk JSON 对象，包含 choices 与可选 usage。
;; stream-state : openai-stream-state | vector
;; 可变流状态；record 为当前表示，vector 用于兼容旧调用。
;; on-event : procedure | #f
;; 接收流式事件的回调；为 #f 时不派发事件。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用更新流状态。

(import (liii check)
  (liii pi ai message)
  (liii pi ai api openai-completions stream-parse)
  (liii pi ai api openai-completions stream-state)
) ;import

(check-set-mode! 'report-failed)

;; 处理文本 delta 与 usage
(let* ((stream-state (make-openai-stream-state '() #f '() "stop" #f '()))
       (events '())
       (emit (lambda (ev) (set! events (append events (list ev)))))
       (chunk (list (cons "choices"
                          (vector (list (cons "index" 0)
                                        (cons "delta" (list (cons "content" "Hi"))))))
                    (cons "usage" (list (cons "prompt_tokens" 10)
                                        (cons "completion_tokens" 2)
                                        (cons "total_tokens" 12)))))
      ) ;
  (process-openai-chunk chunk stream-state emit)
  (check (length (openai-stream-state-blocks stream-state)) => 1)
  (check (pi-text-block-text (car (openai-stream-state-blocks stream-state))) => "Hi")
  (check (cdr (assoc 'input (openai-stream-state-usage stream-state))) => 10)
  (check (length events) => 2)
) ;let*

;; 处理 finish_reason
(let ((stream-state (make-openai-stream-state '() #f '() "stop" #f '()))
      (chunk (list (cons "choices"
                         (vector (list (cons "index" 0)
                                       (cons "delta" '())
                                       (cons "finish_reason" "length"))))))
     ) ;
  (process-openai-chunk chunk stream-state #f)
  (check (openai-stream-state-stop-reason stream-state) => "length")
) ;let*

(check-report)
