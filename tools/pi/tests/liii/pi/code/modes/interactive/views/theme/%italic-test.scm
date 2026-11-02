;; %italic
;; 斜体排版 ANSI 控制码。
;;
;; 语法
;; ----
;; %italic
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

(check-true (string? %italic))
(check-true (string-contains? %italic "\x1b;["))

(check-report)
