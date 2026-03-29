(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; list-take-right 函数测试
;;
;; Scala风格的take-right函数，对越界情况容忍。
;;
;; 语法
;; ----
;; (list-take-right lst n)
;;
;; 参数
;; ----
;; lst : list?
;; 源列表，从中提取元素。
;;
;; n : integer?
;; 要从尾部提取的元素数量。
;;
;; 说明
;; ----
;; 与SRFI-1的take-right不同，list-take-right对越界情况容忍：
;; - 当n < 0时，返回空列表
;; - 当n >= 列表长度时，返回原列表
;; - 否则返回后n个元素
;;
;; 错误处理
;; --------
;; type-error 当lst不是列表或n不是整数时抛出。
;;
;; 示例
;; ----
;; (list-take-right '(1 2 3 4 5) 3) => '(3 4 5)
;; (list-take-right '(1 2 3 4 5) 0) => '()
;; (list-take-right '(1 2 3 4 5) 5) => '(1 2 3 4 5)
;; (list-take-right '(1 2 3) -1) => '()
;; (list-take-right '(1 2 3) 10) => '(1 2 3)

; 基本功能测试
(check (list-take-right '(1 2 3 4 5) 3) => '(3 4 5))
(check (list-take-right '(1 2 3 4 5) 0) => '())
(check (list-take-right '(1 2 3 4 5) 5) => '(1 2 3 4 5))

; 边界容忍测试
(check (list-take-right '(1 2 3) -1) => '())
(check (list-take-right '(1 2 3) 10) => '(1 2 3))

; 空列表测试
(check (list-take-right '() 0) => '())

; 错误处理测试
(check-catch 'type-error (list-take-right "not a list" 2))
(check-catch 'type-error (list-take-right '(1 2 3) "not a number"))

(check-report)
