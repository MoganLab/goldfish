;; raw-string-literal-source
;; 获取原始字符串字面量的源码文本。
;;
;; 语法
;; ----
;; (raw-string-literal-source literal)
;;
;; 参数
;; ----
;; literal : raw-string-literal?
;; 目标字面量记录。
;;
;; 返回值
;; ----
;; string?
;; 字面量的源码文本。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

(check (raw-string-literal-source (make-raw-string-literal "#\"abc\"" "abc"))
  =>
  "#\"abc\""
) ;check

(check-report)
