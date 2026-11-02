;; %assistant-error-text
;; 从助手消息中提取错误提示文本。
;;
;; 语法
;; ----
;; (%assistant-error-text msg)
;;
;; 参数
;; ----
;; msg : list?
;; 助手消息对象。
;;
;; 返回值
;; ----
;; string? or boolean?
;; stopReason=error 时返回错误文本，若无文本兜底为 "Error"；非 error 时返回 #f。

(import (scheme base)
  (liii check)
  (liii pi ai message)
  (liii pi code modes interactive worker-emit)
) ;import

(check-set-mode! 'report-failed)

;; 1. 存在 stopReason="error" 且有具体错误信息
(check (%assistant-error-text (pi-assistant-message '() :stop-reason "error" :error-message "rate limited")
       ) ;%assistant-error-text
  =>
  "rate limited"
) ;check

;; 2. 存在 stopReason="error" 但无 errorMessage 兜底 "Error"
(check (%assistant-error-text (pi-assistant-message '() :stop-reason "error"))
  =>
  "Error"
) ;check

;; 3. 非 error 返回 #f
(check-false (%assistant-error-text (pi-assistant-message '() :stop-reason "stop"))
) ;check-false

(check-report)
