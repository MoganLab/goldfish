;; gemini-stream-state-usage-set!
;; 设置 Gemini 流状态中的用量统计。
;;
;; 语法
;; ----
;; (gemini-stream-state-usage-set! state value)
;;
;; 参数
;; ----
;; state : gemini-stream-state
;; 流状态记录。
;; value : alist
;; 内部 usage alist，包含 input、output、cacheRead、cacheWrite 与 totalTokens。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用更新流状态。


(import (liii check) (liii pi ai api google-generative-ai stream-state))

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "")))
  (gemini-stream-state-usage-set! state
    '((input . 4)
      (output . 5)
      (cacheRead . 0)
      (cacheWrite . 0)
      (totalTokens . 9))
  ) ;gemini-stream-state-usage-set!
  (check (gemini-stream-state-usage state)
    =>
    '((input . 4)
      (output . 5)
      (cacheRead . 0)
      (cacheWrite . 0)
      (totalTokens . 9))
  ) ;check
) ;let


(check-report)
