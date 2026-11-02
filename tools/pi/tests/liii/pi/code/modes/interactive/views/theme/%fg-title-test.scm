;; %fg-title
;; 卡片标题前景色与加粗 ANSI 码。
;;
;; 语法
;; ----
;; %fg-title
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

(check-true (string? %fg-title))
(check-true (string-contains? %fg-title "\x1b;["))

(check-report)
