;; char-literal-source
;; 获取字符字面量的源码文本。
;;
;; 语法
;; ----
;; (char-literal-source literal)
;;
;; 参数
;; ----
;; literal : char-literal?
;; 目标字面量记录。
;;
;; 返回值
;; ----
;; string?
;; 字面量的源码文本。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

(check (char-literal-source (make-char-literal "#\\a" #\a)) => "#\\a")

(check-report)
