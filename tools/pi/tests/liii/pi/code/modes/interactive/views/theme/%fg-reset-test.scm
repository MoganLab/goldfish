;; %fg-reset
;; 前景色复位 ANSI 码。
;;
;; 语法
;; ----
;; %fg-reset
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; string?
;; ANSI 转义序列字符串。

(import (scheme base)
  (liii check)
  (liii string)
  (liii pi code modes interactive views theme)
) ;import

(check-set-mode! 'report-failed)

(check-true (string? %fg-reset))
(check-true (string-contains? %fg-reset "\x1b;["))

(check-report)
