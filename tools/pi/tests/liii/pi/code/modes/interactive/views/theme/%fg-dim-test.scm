;; %fg-dim
;; 弱化暗色前景色 ANSI 码。
;;
;; 语法
;; ----
;; %fg-dim
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

(check-true (string? %fg-dim))
(check-true (string-contains? %fg-dim "\x1b;["))

(check-report)
