;; openai-stream-state-tools
;; 读取 OpenAI 流状态中正在拼接的工具调用表。
;;
;; 语法
;; ----
;; (openai-stream-state-tools state)
;;
;; 参数
;; ----
;; state : openai-stream-state | vector
;; 流状态；record 为当前表示，vector 用于兼容旧调用。
;;
;; 返回值
;; ----
;; list
;; tool_call_state 关联表；没有待拼接工具时为空列表。

(import (liii check)
  (liii pi ai api openai-completions stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-openai-stream-state '() #f '() "stop" #f '())))
  (check (openai-stream-state? state) => #t)
  (check (openai-stream-state-tools state) => '())
) ;let*

;; vector 状态（兼容旧调用）
(let ((state (vector '() #f '() "stop" #f '())))
  (check (openai-stream-state-tools state) => '())
) ;let*

(check-report)
