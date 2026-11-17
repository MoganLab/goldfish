;; gemini-stream-state-current-active-type-set!
;; 设置 Gemini 流状态中当前正在进行的块类型。
;;
;; 语法
;; ----
;; (gemini-stream-state-current-active-type-set! state value)
;;
;; 参数
;; ----
;; state : gemini-stream-state
;; 流状态记录。
;; value : string | #f
;; 当前活动块类型，可取 text 或 thinking；没有活动块时为 #f。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用更新流状态。


(import (liii check) (liii pi ai api google-generative-ai stream-state))

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "")))
  (gemini-stream-state-current-active-type-set! state "text")
  (check (gemini-stream-state-current-active-type state) => "text")
) ;let


(check-report)
