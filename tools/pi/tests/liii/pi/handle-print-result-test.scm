(import (liii check)
        (liii pi)
        (liii pi modes))

(check-set-mode! 'report-failed)

;; 正常 assistant 记录：输出文本并返回 0
(check (handle-print-result
         (pi-assistant-message (list (pi-text-block "hello out")))
       ) ;handle-print-result
  => 0
) ;check

;; provider 错误响应为 alist 时也能给出可读错误
(check (handle-print-result
         (list (cons 'role "assistant")
           (cons 'content '())
           (cons 'stopReason "error")
           (cons 'errorMessage "boom")
         ) ;list
       ) ;handle-print-result
  => 1
) ;check

(check-report)
