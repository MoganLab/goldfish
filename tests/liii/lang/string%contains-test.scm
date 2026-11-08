;; string%contains
;; 判断字符串是否包含指定字符（对标 Scala String.contains(Char)）。
;;
;; 语法
;; ----
;; ($ s :contains c)
;;
;; 参数
;; ----
;; c : char?
;; 待查找字符。
;;
;; 返回值
;; ------
;; boolean
;; 含该字符则 #t。UTF-8 语义，中文/emoji 按单字符比较；
;; 非字符入参返回 #f（Scala contains(Char) 的类型约束对应行为）。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ "héllo" :contains #\l) => #t)
(check ($ "héllo" :contains #\z) => #f)
(check ($ "测试" :contains #\测) => #t)
(check ($ "" :contains #\a) => #f)
(check ($ "abc" :contains "ab") => #f)

(check-report)
