;; handle-print-result
;; 输出 print 模式的最终结果并返回退出状态码。
;;
;; 语法
;; ----
;; (handle-print-result last-msg)
;;
;; 参数
;; ----
;; last-msg : any
;; 最后一条响应消息，可为消息对象、alist 或 #f。
;;
;; 返回值
;; ----
;; integer
;; 正常输出返回 0，请求失败返回 1。

(import (liii check)
  (liii pi ai message)
  (liii pi code modes output)
  (liii pi code core tools)
  (liii pi code modes)
) ;import

(check-set-mode! 'report-failed)

;; 正常 assistant 记录：输出文本并返回 0
(check
  (handle-print-result (pi-assistant-message (list (pi-text-block "hello out"))))
  =>
  0
) ;check

;; provider 错误响应为 alist 时也能给出可读错误
(check (handle-print-result (list (cons 'role "assistant")
                              (cons 'content '())
                              (cons 'stopReason "error")
                              (cons 'errorMessage "boom")
                            ) ;list
       ) ;handle-print-result
  =>
  1
) ;check

(check-report)
