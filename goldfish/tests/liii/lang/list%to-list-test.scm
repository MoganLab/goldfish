;; list%to-list / vector%to-list / list%to-vector / vector%to-vector
;; 集合类型间互转（对标 Scala toList / toVector）。
;;
;; 语法
;; ----
;; ($ coll :to-list)
;; ($ coll :to-vector)

;; 参数
;; ----
;; coll : list?/vector?/string?/option?
;; 待转换的集合或 option。
;;
;; 返回值
;; ------
;; list? / vector?
;; :to-list 返回链表；:to-vector 返回向量。string 按字符、
;; option 按单元素（空为空集）转换。
;;
(import (liii check) (liii lang) (liii option))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3) :to-vector) => '#(1 2 3))
(check ($ #(1 2 3) :to-list) => '(1 2 3))
(check ($ '(1 2 3) :to-list) => '(1 2 3))
(check ($ #(1 2 3) :to-vector) => '#(1 2 3))
(check ($ "abc" :to-list) => '(#\a #\b #\c))
(check ($ "abc" :to-vector) => '#(#\a #\b #\c))
(check ($ (option 5) :to-list) => '(5))
(check ($ (none) :to-list) => '())
(check ($ (option 5) :to-vector) => '#(5))
(check ($ (none) :to-vector) => '#())

(check-report)
