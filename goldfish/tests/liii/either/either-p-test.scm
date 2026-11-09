;; either?
;; 判断对象是否为 Either 值（Left 或 Right）。
;;
;; 语法
;; ----
;; (either? obj)
;;
;; 参数
;; ----
;; obj : any
;; 要判断的任意对象。
;;
;; 返回值
;; ----
;; boolean
;; obj 为 Left 或 Right 时返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; Either 用 (cons value 'left) / (cons value 'right) 表示，
;; 因此普通点对、字符串等即使结构相似也不会被判为 Either。
;; either? 是 to-left、to-right、either-map 等函数的类型安全检查基础。

(import (liii check) (liii either))

(check (either? (from-left "error")) => #t)
(check (either? (from-right 42)) => #t)

;; 值为假时仍然是 Either，判据只看向标记
(check (either? (from-left '())) => #t)
(check (either? (from-right #f)) => #t)

;; 非 Either：普通点对、空表、标量与字符串
(check (either? (cons 1 'other)) => #f)
(check (either? '()) => #f)
(check (either? "left") => #f)
(check (either? 42) => #f)
(check (either? #f) => #f)
(check (either? '(1 2)) => #f)

(check-report)
