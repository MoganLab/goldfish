;; pi-format-size
;; 把字节数格式化为人类可读的尺寸字符串
;;
;; 语法
;; ----
;; (pi-format-size n)
;;
;; 参数
;; ----
;; n : integer?，字节数
;;
;; 返回值
;; ------
;; string?，如 "1.2K"

(import (liii check) (liii pi code core tools))

(check-set-mode! 'report-failed)

;; pi-format-size
;; 格式化字节大小为人类可读字符串。
(check (pi-format-size 500) => "500B")
(check (pi-format-size 1024) => "1.0KB")
(check (pi-format-size 1536) => "1.5KB")

(check-report)
