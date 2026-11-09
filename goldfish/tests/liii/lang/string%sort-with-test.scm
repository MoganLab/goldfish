;; string%sort-with
;; 字符串按比较器排序（对标 Scala String.sorted / sortWith）。
;;
;; 语法
;; ----
;; ($ s :sort-with cmp)
;;
;; 参数
;; ----
;; cmp : procedure?
;; 字符比较函数，如 char<?。
;;
;; s : string?
;; 源字符串。
;;
;; 返回值
;; ------
;; string?
;; 排序后的新字符串。UTF-8 语义，按字符比较。

(import (liii check) (liii lang) (scheme char))

(check-set-mode! 'report-failed)

(check ($ "cab" :sort-with char<?) => "abc")
(check ($ "" :sort-with char<?) => "")
(check ($ "b" :sort-with char<?) => "b")

(check-report)
