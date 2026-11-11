;; gemini-stream-state-tool-calls-set!
;; 设置 Gemini 流状态中已完成的工具调用列表。
;;
;; 语法
;; ----
;; (gemini-stream-state-tool-calls-set! state value)
;;
;; 参数
;; ----
;; state : gemini-stream-state | vector
;; 流状态；record 为当前表示，vector 用于兼容旧调用。
;; value : list
;; 已完成的 pi-tool-call-block 列表。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用更新流状态。

;; 说明
;; ----
;; record 状态直接更新记录字段；vector 状态更新对应的下标。

(import (liii check) (liii pi ai api google-generative-ai stream-state))

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "")))
  (gemini-stream-state-tool-calls-set! state '())
  (check (gemini-stream-state-tool-calls state) => '())
) ;let

;; vector 状态（兼容旧调用）
(let ((state (vector '() #f '() '() "STOP" #f "" "")))
  (gemini-stream-state-tool-calls-set! state '())
  (check (gemini-stream-state-tool-calls state) => '())
) ;let

(check-report)
