;; PI_HELP_TEXT
;; Pi CLI 命令行工具的使用帮助文本常量。
;;
;; 语法
;; ----
;; PI_HELP_TEXT
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; string
;; 完整的帮助文本字符串。

(import (liii raw-string)
  (liii pi code cli help)
  (liii check)
  (liii string)
) ;import

(check-set-mode! 'report-failed)

(check-true (string? PI_HELP_TEXT))
(check-true (string-contains? PI_HELP_TEXT "gf pi - AI coding assistant script mode"))
(check-true (string-contains? PI_HELP_TEXT "--model"))
(check-true (string-contains? PI_HELP_TEXT "--thinking"))

(check-report)
