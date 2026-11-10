;; flatten
;; 展平嵌套列表到指定深度，或者展平到最深层级。
;;
;; 语法
;; ----
;; (flatten lst)
;; (flatten lst depth)
;;
;; 参数
;; ----
;; lst : list
;; 被展平的列表。
;;
;; depth : integer 或 'deepest
;; 展平深度。当为整数时表示展平的层数，当为 'deepest 时表示展平到最深层级。
;;
;; 返回值
;; ------
;; list
;; 展平后的列表。如果指定 depth，将按指定深度展平嵌套结构。如果指定 'deepest，将完全展平所有嵌套层级。

(import (liii list) (liii check))

(check-set-mode! 'report-failed)

(check (flatten '((a) () (b ()) () (c)) 'deepest) => '(a b c))
(check (flatten '((a b) c (((d)) e)) 'deepest) => '(a b c d e))
(check (flatten '(a b (() (c))) 'deepest) => '(a b c))

(check (flatten '((a) () (b ()) () (c)) 0) => '((a) () (b ()) () (c)))
(check (flatten '((a) () (b ()) () (c)) 1) => '(a b () c))
(check (flatten '((a) () (b ()) () (c))) => '(a b () c))
(check (flatten '((a) () (b ()) () (c)) 2) => '(a b c))
(check (flatten '((a) () (b ()) () (c)) -1) => '(a b c))
(check (flatten '((a b) c (((d)) e)) 0) => '((a b) c (((d)) e)))
(check (flatten '((a b) c (((d)) e)) 1) => '(a b c ((d)) e))
(check (flatten '((a b) c (((d)) e))) => '(a b c ((d)) e))
(check (flatten '((a b) c (((d)) e)) 2) => '(a b c (d) e))
(check (flatten '((a b) c (((d)) e)) 3) => '(a b c d e))
(check (flatten '((a b) c (((d)) e)) -1) => '(a b c d e))
(check (flatten '(a b (() (c))) 0) => '(a b (() (c))))
(check (flatten '(a b (() (c))) 1) => '(a b () (c)))
(check (flatten '(a b (() (c)))) => '(a b () (c)))
(check (flatten '(a b (() (c))) 2) => '(a b c))
(check (flatten '(a b (() (c))) -1) => '(a b c))

(check-catch 'type-error (flatten '((a) () (b ()) () (c)) 'a))
(check-catch 'type-error (flatten '((a) () (b ()) () (c)) (make-vector 1 1)))

(check-report)
