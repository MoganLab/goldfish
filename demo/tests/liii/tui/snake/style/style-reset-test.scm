;; style-reset
;; 返回 ANSI 样式重置转义序列
;;
;; 语法
;; ----
;; (style-reset)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; string?\nANSI SGR 重置序列 "\x1b;[0m"

(import (scheme base)
  (liii check)
  (liii tui snake style)
) ;import

(check-set-mode! 'report-failed)

(check (string=? (style-reset) "\x1b;[0m") => #t)
(check-true (string? (style-reset)))

(check-report)
