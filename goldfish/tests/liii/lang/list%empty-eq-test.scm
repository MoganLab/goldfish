;; list%empty? / list%not-empty?
;; 判空操作。
;;
;; 语法
;; ----
;; ($ xs :empty?)
;; ($ xs :not-empty?)

;; 参数
;; ----
;; xs : list?
;; 被检查的集合。
;;
;; 返回值
;; ------
;; boolean
;; :empty? 空集合为 #t；:not-empty? 非空为 #t。list/vector/string/option 同构。
;;
(import (liii check) (liii lang) (liii option))

(check-set-mode! 'report-failed)

;; list
(check ($ '(1 2 3) :empty?) => #f)
(check ($ '() :empty?) => #t)
(check ($ '(1) :not-empty?) => #t)
(check ($ '() :not-empty?) => #f)

;; vector
(check ($ #(1 2) :empty?) => #f)
(check ($ #() :empty?) => #t)

;; string (UTF-8)
(check ($ "" :empty?) => #t)
(check ($ "a" :empty?) => #f)
(check ($ "测试" :not-empty?) => #t)

;; option
(check ($ (none) :empty?) => #t)
(check ($ (option 42) :empty?) => #f)
(check ($ (option 42) :not-empty?) => #t)

(check-report)
