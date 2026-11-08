;; string%zip
;; 与另一字符串按短侧交错配对（对标 Scala String.zip 的拼接形态）。
;;
;; 语法
;; ----
;; ($ s :zip other)
;;
;; 参数
;; ----
;; other : string?
;; 配对字符串，短侧对齐。
;;
;; s : string?
;; 源字符串。
;;
;; 返回值
;; ------
;; string?
;; 两串字符逐位交替拼接的结果，长度取两者较短者的两倍。UTF-8 语义。
;;
;; 说明
;; ----
;; Scala "ab".zip("cd") 产生二元组序列；本实现对 string 主语返回
;; 交替拼接串（(a1 b1)(a2 b2) 字符序列的字符串形态）。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ "ab" :zip "cd") => "acbd")
(check ($ "a" :zip "bc") => "ab")
(check ($ "" :zip "ab") => "")

(check-report)
