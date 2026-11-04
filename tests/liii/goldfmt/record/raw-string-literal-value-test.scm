;; raw-string-literal-value
;; 获取原始字符串字面量解析后的字符串值。
;;
;; 语法
;; ----
;; (raw-string-literal-value literal)
;;
;; 参数
;; ----
;; literal : raw-string-literal?
;; 目标字面量记录。
;;
;; 返回值
;; ----
;; string?
;; 字面量解析后的字符串值。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

(check (raw-string-literal-value (make-raw-string-literal "#\"abc\"" "abc"))
  =>
  "abc"
) ;check

(check-report)
