(import (liii check) (liii pi prompt context))

(check-set-mode! 'report-failed)

;; pi-resolve-prompt-input
;; 解析直接字符串输入或文件引用输入。
(check (pi-resolve-prompt-input "Hello prompt") => "Hello prompt")

(check-report)
