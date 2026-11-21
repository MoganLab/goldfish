;; and
;; 对任意数量的参数执行逻辑与操作，支持短路求值。
;;
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
;; ----
;; any
;; 如果没有任何表达式，返回 #t
;; 如果只有一个表达式，返回该表达式的结果
;; 对于多个表达式，返回最后一个真值表达式的结果，或者遇到第一个假值时立即返回 #f
;;
;; 说明
;; ----
;; 从左到右依次求值，一旦遇到 #f 就立即停止求值并返回 #f

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; [CI-DEBUG] 逐行打点定位 SIGSEGV，事后移除
(define (dbg!/mark n) (display "[dbg]check ") (display n) (newline) (flush-output-port (current-output-port)))

;; 基础测试用例
(dbg!/mark 1)
(check-true (and))
(dbg!/mark 2)
(check (and 1) => 1)
(dbg!/mark 3)
(check-false (and #f))
;; 多参数真值情况
(dbg!/mark 4)
(check-true (and #t #t #t))
(dbg!/mark 5)
(check (and 1 2 3) => 3)
(dbg!/mark 6)
(check (and #t "string" 'symbol) => 'symbol)
;; 多参数假值情况
(dbg!/mark 7)
(check-false (and #t #f #t))
(dbg!/mark 8)
(check-false (and #f #t #f))
(dbg!/mark 9)
(check-false (and #f #f #f))
;; 混合类型测试
(dbg!/mark 10)
(check-true (and 1 '() "non-empty" #t))
(dbg!/mark 11)
(check-false (and #f '() "non-empty" #t))
(dbg!/mark 12)
(check-false (and 1 '() "non-empty" #f))
;; 表达式求值测试
(dbg!/mark 13)
(check-true (and (> 5 3) (< 5 10)))
(dbg!/mark 14)
(check-false (and (> 5 3) (> 5 10)))
;; 短路求值测试
(dbg!/mark 15)
(dbg!/mark 15.1)
(check-catch 'error-name
  (and (error 'error-name "This should not be evaluated") #f)
) ;check-catch
(dbg!/mark 15.2)
(check-false (and #f (error "This should not be evaluated")))
(dbg!/mark 16)
;; 边缘情况测试
(check (and 0) => 0)
(dbg!/mark 17)
(check (and '()) => '())
(dbg!/mark 18)
(check (and #t #t '()) => '())
(dbg!/mark 19)
(check-false (and #t #t #f #t))
;; 确保返回的是原始值而非转换后的布尔值
(dbg!/mark 20)
(check (and #t 42) => 42)
(dbg!/mark 21)
(check (and #t 'a 'b 'c) => 'c)
(dbg!/mark 22)
(check-false (and 'a 'b #f 'd))
(dbg!/mark 23)

(dbg!/mark 24)

(check-report)
