;; gemini-stream-state-usage
;; 读取 Gemini 流状态中的用量统计。
;;
;; 语法
;; ----
;; (gemini-stream-state-usage state)
;;
;; 参数
;; ----
;; state : gemini-stream-state | vector
;; 流状态；record 为当前表示，vector 用于兼容旧调用。
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

;; vector 状态（兼容旧调用）
(let ((state (vector '()
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
             ) ;vector
      ) ;state
     ) ;
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
