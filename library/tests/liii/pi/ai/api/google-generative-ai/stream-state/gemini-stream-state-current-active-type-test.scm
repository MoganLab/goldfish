;; gemini-stream-state-current-active-type
;; 读取 Gemini 流状态中当前正在进行的块类型。
;;
;; 语法
;; ----
;; (gemini-stream-state-current-active-type state)
;;
;; 参数
;; ----
;; state : gemini-stream-state | vector
;; 流状态；record 为当前表示，vector 用于兼容旧调用。
;;
;; 返回值
;; ----
;; string | #f
;; 当前活动块类型，可取 text 或 thinking；没有活动块时为 #f。

(import (liii check)
  (liii pi ai api google-generative-ai stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "")))
  (check (gemini-stream-state? state) => #t)
  (check (gemini-stream-state-current-active-type state) => #f)
) ;let*

;; vector 状态（兼容旧调用）
(let ((state (vector '() #f '() '() "STOP" #f "" "")))
  (check (gemini-stream-state-current-active-type state) => #f)
) ;let*

(check-report)
