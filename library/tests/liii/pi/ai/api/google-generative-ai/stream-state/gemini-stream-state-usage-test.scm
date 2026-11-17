;; gemini-stream-state-usage
;; 读取 Gemini 流状态中的用量统计。
;;
;; 语法
;; ----
;; (gemini-stream-state-usage state)
;;
;; 参数
;; ----
;; state : gemini-stream-state
;; 流状态记录。
;;
;; 返回值
;; ----
;; alist
;; 内部 usage alist，包含 input、output、cacheRead、cacheWrite 与 totalTokens。

(import (liii check) (liii pi ai api google-generative-ai stream-state))

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-gemini-stream-state '()
               #f
               '((input . 1)
                 (output . 2)
                 (cacheRead . 0)
                 (cacheWrite . 0)
                 (totalTokens . 3))
               '()
               "STOP"
               #f
               ""
               ""
             ) ;make-gemini-stream-state
      ) ;state
     ) ;
  (check (gemini-stream-state? state) => #t)
  (check (gemini-stream-state-usage state)
    =>
    '((input . 1)
      (output . 2)
      (cacheRead . 0)
      (cacheWrite . 0)
      (totalTokens . 3))
  ) ;check
) ;let


(check-report)
