;; %symbol-keyword?
;; 判断对象是否为以冒号开头的选项关键字符号。
;;
;; 语法
;; ----
;; (%symbol-keyword? sym)
;;
;; 参数
;; ----
;; sym : any
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; sym 是以 #\: 开头的 symbol 时返回 #t，否则返回 #f。

(import (liii check) (liii subprocess run))

(check-set-mode! 'report-failed)

(check (%symbol-keyword? :cwd) => #t)
(check (%symbol-keyword? 'cwd) => #f)
(check (%symbol-keyword? ':) => #t)
(check (%symbol-keyword? ":cwd") => #f)
(check (%symbol-keyword? 42) => #f)

(check-report)
