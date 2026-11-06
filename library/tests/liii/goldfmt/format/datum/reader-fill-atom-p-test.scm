;; reader-fill-atom?
;; 判断元素是否可以参与 fill 布局。
;;
;; 语法
;; ----
;; (reader-fill-atom? item)
;;
;; 参数
;; ----
;; item : any?
;; 待检查的元素。
;;
;; 返回值
;; ----
;; boolean?
;; 返回 #t 表示 item 是非列表、非向量的原子值。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(check (reader-fill-atom? 'a) => #t)
(check (reader-fill-atom? '(a)) => #f)
(check (reader-fill-atom? #(a)) => #f)

(check-report)
