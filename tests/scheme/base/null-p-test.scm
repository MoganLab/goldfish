(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)

;;
;; null?
;; 判断给定的对象是否为空列表。
;;
;; 语法
;; ----
;; (null? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意类型的对象
;;
;; 返回值
;; ------
;; boolean?
;; 如果obj是空列表则返回#t，否则返回#f
;;
;; 说明
;; ----
;; 1. 用于检查对象是否为空列表'()
;; 2. 对其他任何类型的对象都返回#f
;; 3. 通常在列表处理中使用，用于判断列表是否为空
;;
;; 特殊规则
;; ---------
;; - 仅当参数为精确的空列表 '() 时返回 #t
;; - 所有其他对象，包括向量、字符串、数字等都返回 #f
;; - 非列表结构也返回 #f（如点对、符号等）
;;
;; 错误处理
;; ---------
;; wrong-number-of-args
;; 当参数数量不为1时抛出错误。

;; null? 基本测试：空列表和非空列表
(check (null? '()) => #t)
(check (null? '(1)) => #f)
(check (null? '(a)) => #f)
(check (null? '(a b c)) => #f)
(check (null? '(1 2 3 4 5)) => #f)
;; null? 特殊结构和边界情况
(check (null? '(())) => #f)
(check (null? '(() () ())) => #f)
(check (null? '((a b) (c d))) => #f)
;; null? 非列表类型测试 - 全面覆盖
(check (null? #t) => #f)
(check (null? #f) => #f)
(check (null? 0) => #f)
(check (null? 123) => #f)
(check (null? -456) => #f)
(check (null? 3.14) => #f)
(check (null? "") => #f)
(check (null? "hello") => #f)
(check (null? '#()) => #f)
(check (null? '#(1 2 3)) => #f)
(check (null? 'symbol) => #f)
(check (null? '123) => #f)
(check (null? #\a) => #f)
;; null? 点对结构测试
(check (null? '(a . b)) => #f)
(check (null? (cons 1 2)) => #f)
;; null? 复杂表达式测试
(check (null? (list)) => #t)
(check (null? (append '() '())) => #t)
(check (null? (cdr '(a))) => #t)
(check (null? (cdr '(a b))) => #f)
;; null? 与列表操作结合测试
(check (null? (reverse '())) => #t)
(check (null? (reverse '(1))) => #f)
;; null? 错误处理测试
(check-catch 'wrong-number-of-args
  (null?)
) ;check-catch
(check-catch 'wrong-number-of-args
  (null? '() '())
) ;check-catch
(check-catch 'wrong-number-of-args
  (null? 1 2)
) ;check-catch
(check-report)