;; vector%mk-string
;; 将向量元素拼接为字符串（对标 Scala Vector.mkString）。
;;
;; 语法
;; ----
;; ($ v :mk-string)
;; ($ v :mk-string sep)
;; ($ v :mk-string start sep end)
;;
;; 参数
;; ----
;; sep : string?
;; 可选，元素间分隔符。
;;
;; start : string?
;; 可选，前缀。
;;
;; end : string?
;; 可选，后缀。
;;
;; v : vector?
;; 源向量。
;;
;; 返回值
;; ------
;; string?
;; 拼接结果；空向量返回 ""。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2) :mk-string "-") => "1-2")
(check ($ #(1 2 3) :mk-string) => "123")
(check ($ #() :mk-string ",") => "")
(check ($ #(1 2 3) :mk-string "<" "," ">") => "<1,2,3>")

(check-report)
