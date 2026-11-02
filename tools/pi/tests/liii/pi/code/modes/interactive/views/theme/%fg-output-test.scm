;; %fg-output
;; 工具输出前景色 ANSI 码。
;;
;; 语法
;; ----
;; %fg-output
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

(check-true (string? %fg-output))
(check-true (string-contains? %fg-output "\x1b;["))

(check-report)
