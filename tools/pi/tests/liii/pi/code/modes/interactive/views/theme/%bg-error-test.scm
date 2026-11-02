;; %bg-error
;; 错误状态卡片背景色 ANSI 码。
;;
;; 语法
;; ----
;; %bg-error
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

(check-true (string? %bg-error))
(check-true (string-contains? %bg-error "\x1b;["))

(check-report)
