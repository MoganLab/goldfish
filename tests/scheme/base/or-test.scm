(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; or
;; 对任意数量的参数执行逻辑或操作，支持短路求值。
;;
;; 语法
;; ----
;; (or [expr ...])
;;
;; 参数
;; ----
;; expr : any
;; 任意类型的表达式。在 Scheme 中，除了 #f 之外的所有值都被视为真值。
;;
;; 返回值
;; -----
;; any
;; 如果没有任何表达式，返回 #f
;; 如果只有一个表达式，返回该表达式的结果
;; 对于多个表达式，返回第一个真值表达式的结果，如果都为假则返回最后一个值
;;
;; 短路求值
;; -------
;; 从左到右依次求值，一旦遇到真值就立即停止求值并返回该值
;; 基础测试用例
(check-false (or))
(check-true (or #t))
(check-false (or #f))
;; 多参数真值情况
(check-true (or #t #t #t))
(check (or #t #f #t) => #t)
(check (or #f #t #f) => #t)
;; 多参数假值情况
(check-false (or #f #f #f))
;; 混合类型测试 - 返回第一个真值
(check (or 1 '() "non-empty" #t) => 1)
(check (or #f '() "non-empty" #t)
  =>
  '()
) ;check
(check (or #f #f 2 #t) => 2)
;; 表达式求值测试
(check-true (or (> 5 3) (< 5 10)))
(check-true (or (> 5 3) (> 5 10)))
(check-false (or (< 5 3) (> 5 10)))
;; 短路求值测试 - 一旦遇到真值就停止求值
(check-true (or #t
              (error "This should not be evaluated")
            ) ;or
) ;check-true
;; 返回第一个真值
(check (or #f 1) => 1)
(check (or #f #f 2) => 2)
(check (or #f #f #f) => #f)
(check-report)
