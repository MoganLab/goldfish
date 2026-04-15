(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; and
;; 对任意数量的参数执行逻辑与操作，支持短路求值。
;;
;; 语法
;; ----
;; (and [expr ...])
;;
;; 参数
;; ----
;; expr : any
;; 任意类型的表达式。在 Scheme 中，除了 #f 之外的所有值都被视为真值。
;;
;; 返回值
;; -----
;; any
;; 如果没有任何表达式，返回 #t
;; 如果只有一个表达式，返回该表达式的结果
;; 对于多个表达式，返回最后一个真值表达式的结果，或者遇到第一个假值时立即返回 #f
;;
;; 短路求值
;; -------
;; 从左到右依次求值，一旦遇到 #f 就立即停止求值并返回 #f
;; 基础测试用例
(check-true (and))
(check (and 1) => 1)
(check-false (and #f))
;; 多参数真值情况
(check-true (and #t #t #t))
(check (and 1 2 3) => 3)
(check (and #t "string" 'symbol)
  =>
  'symbol
) ;check
;; 多参数假值情况
(check-false (and #t #f #t))
(check-false (and #f #t #f))
(check-false (and #f #f #f))
;; 混合类型测试
(check-true (and 1 '() "non-empty" #t))
(check-false (and #f '() "non-empty" #t)
) ;check-false
(check-false (and 1 '() "non-empty" #f))
;; 表达式求值测试
(check-true (and (> 5 3) (< 5 10)))
(check-false (and (> 5 3) (> 5 10)))
;; 短路求值测试
(check-catch 'error-name
  (and (error 'error-name
         "This should not be evaluated"
       ) ;error
    #f
  ) ;and
) ;check-catch
(check-false (and #f
               (error "This should not be evaluated")
             ) ;and
) ;check-false
;; 边缘情况测试
(check (and 0) => 0)
(check (and '()) => '())
(check (and #t #t '()) => '())
(check-false (and #t #t #f #t))
;; 确保返回的是原始值而非转换后的布尔值
(check (and #t 42) => 42)
(check (and #t 'a 'b 'c) => 'c)
(check-false (and 'a 'b #f 'd))
(check-report)