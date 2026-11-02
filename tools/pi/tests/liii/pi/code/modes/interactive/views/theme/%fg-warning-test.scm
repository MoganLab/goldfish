;; %fg-warning
;; 警告提示前景色 ANSI 码。
;;
;; 语法
;; ----
;; %fg-warning
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

(check-true (string? %fg-warning))
(check-true (string-contains? %fg-warning "\x1b;["))

(check-report)
