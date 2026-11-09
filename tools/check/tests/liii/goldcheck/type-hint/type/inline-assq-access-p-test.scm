;; inline-assq-access?
;; 判断表达式是否为 (cdr (assq/assoc ...)) 内联取值惯用法。
;;
;; 语法
;; ----
;; (inline-assq-access? expr)
;;
;; 参数
;; ----
;; expr : 表达式
;;
;; 返回值
;; ------
;; boolean
;; 是 accessor 直接作用于 assq/assoc 结果的形态时 #t。

(import (scheme base) (liii check) (liii goldcheck type-hint type))

(check-set-mode! 'report-failed)

;; 1. 命中：accessor + assq/assoc
(check-true (inline-assq-access? '(cdr (assq 'k a))))
(check-true (inline-assq-access? '(car (assoc "k" a))))
(check-true (inline-assq-access? '(cadr (assq k a))))

;; 2. 不命中：非 accessor、非 assq/assoc、多层访问
(check (inline-assq-access? '(length (assq 'k a))) => #f)
(check (inline-assq-access? '(cdr (memq 'k a))) => #f)
(check (inline-assq-access? '(cdr (cdr (assq 'k a)))) => #f)
(check (inline-assq-access? 'sym) => #f)

(check-report)
