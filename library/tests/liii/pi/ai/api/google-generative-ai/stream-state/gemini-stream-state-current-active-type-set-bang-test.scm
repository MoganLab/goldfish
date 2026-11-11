;; gemini-stream-state-current-active-type-set!
;; 设置 Gemini 流状态中当前正在进行的块类型。
;;
;; 语法
;; ----
;; (gemini-stream-state-current-active-type-set! state value)
;;
;; 参数
;; ----
;; state : gemini-stream-state | vector
;; 流状态；record 为当前表示，vector 用于兼容旧调用。
;; value : string | #f
;; 当前活动块类型，可取 text 或 thinking；没有活动块时为 #f。
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
  (gemini-stream-state-current-active-type-set! state "text")
  (check (gemini-stream-state-current-active-type state) => "text")
) ;let

;; vector 状态（兼容旧调用）
(let ((state (vector '() #f '() '() "STOP" #f "" "")))
  (gemini-stream-state-current-active-type-set! state "text")
  (check (gemini-stream-state-current-active-type state) => "text")
) ;let

(check-report)
