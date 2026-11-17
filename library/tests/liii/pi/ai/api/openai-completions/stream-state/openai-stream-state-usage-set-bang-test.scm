;; openai-stream-state-usage-set!
;; 设置 OpenAI 流状态中的用量统计。
;;
;; 语法
;; ----
;; (openai-stream-state-usage-set! state value)
;;
;; 参数
;; ----
;; state : openai-stream-state
;; 流状态记录。
;; value : alist
;; 内部 usage alist，包含 input、output、cacheRead、cacheWrite 与 totalTokens。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用更新流状态。


(import (liii check)
  (liii pi ai api openai-completions stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-openai-stream-state '() #f '() "stop" #f '())))
  (openai-stream-state-usage-set! state '((input . 4) (output . 5) (cacheRead . 0) (cacheWrite . 0) (totalTokens . 9)))
  (check (openai-stream-state-usage state) => '((input . 4) (output . 5) (cacheRead . 0) (cacheWrite . 0) (totalTokens . 9)))
) ;let*


(check-report)
