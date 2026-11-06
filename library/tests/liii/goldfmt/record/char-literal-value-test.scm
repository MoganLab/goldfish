;; char-literal-value
;; 获取字符字面量解析后的字符值。
;;
;; 语法
;; ----
;; (char-literal-value literal)
;;
;; 参数
;; ----
;; literal : char-literal?
;; 目标字面量记录。
;;
;; 返回值
;; ----
;; char?
;; 字面量解析后的字符值。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

(check (char-literal-value (make-char-literal "#\\a" #\a)) => #\a)

(check-report)
