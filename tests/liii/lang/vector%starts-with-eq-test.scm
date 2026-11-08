;; vector%starts-with? / vector%ends-with?
;; 判断向量是否以指定前缀/后缀开始或结尾（对标 Scala Vector.startsWith / endsWith）。
;;
;; 语法
;; ----
;; ($ v :starts-with? prefix)
;; ($ v :ends-with? suffix)

;; 参数
;; ----
;; prefix : vector?
;; 前缀向量。
;;
;; v : vector?
;; 源向量。
;;
;; 返回值
;; ------
;; boolean
;; 前缀/后缀匹配返回 #t；prefix/suffix 为空向量恒 #t。
;;
(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2 3) :starts-with? #(1)) => #t)
(check ($ #(1 2 3) :starts-with? #()) => #t)
(check ($ #(1 2 3) :starts-with? #(2)) => #f)

(check ($ #(1 2 3) :ends-with? #(2 3)) => #t)
(check ($ #(1 2 3) :ends-with? #()) => #t)
(check ($ #(1 2 3) :ends-with? #(1)) => #f)

(check-report)
