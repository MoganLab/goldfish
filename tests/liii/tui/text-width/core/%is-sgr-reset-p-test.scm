;; %is-sgr-reset?
;; 判断字符串是否为 ANSI SGR 重置序列。
;;
;; 语法
;; ----
;; (%is-sgr-reset? s)
;;
;; 参数
;; ----
;; s : string?，待判断的字符串
;;
;; 返回值
;; ----
;; boolean?，若是 "\x1b[0m" 或 "\x1b[m" 返回 #t，否则返回 #f。

(import (liii check) (liii tui text-width core))

(check-set-mode! 'report-failed)

;; SGR 重置序列
(check (%is-sgr-reset? "\x1b;[0m") => #t)
(check (%is-sgr-reset? "\x1b;[m") => #t)

;; 非重置序列
(check (%is-sgr-reset? "\x1b;[31m") => #f)
(check (%is-sgr-reset? "\x1b;[1m") => #f)
(check (%is-sgr-reset? "") => #f)
(check (%is-sgr-reset? "hello") => #f)

(check-report)
