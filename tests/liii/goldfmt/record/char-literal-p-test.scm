;; char-literal?
;; 判断一个值是否为字符字面量记录。
;;
;; 语法
;; ----
;; (char-literal? value)
;;
;; 参数
;; ----
;; value : any?
;; 待检查的 Scheme 值。
;;
;; 返回值
;; ----
;; boolean?
;; 返回 #t 表示 value 是字符字面量记录；否则返回 #f。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

(check (char-literal? (make-char-literal)) => #t)
(check (char-literal? "string") => #f)
(check (char-literal? #\a) => #f)
(check (char-literal? (make-env)) => #f)

(check-report)
