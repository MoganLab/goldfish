(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; list-take 函数测试
;;
;; Scala风格的take函数，对越界情况容忍。
;;
;; 语法
;; ----
;; (list-take lst n)
;;
;; 参数
;; ----
;; lst : list?
;; 源列表，从中提取元素。
;;
;; n : integer?
;; 要提取的元素数量。
;;
;; 返回值
;; ------
;; list
;; 包含指定数量元素的新列表，从原列表的开头开始计数。
;;
;; 说明
;; ----
;; 与SRFI-1的take不同，list-take对越界情况容忍：
;; - 当n < 0时，返回空列表
;; - 当n >= 列表长度时，返回原列表
;; - 否则返回前n个元素
;;
;; 错误处理
;; --------
;; type-error 当lst不是列表或n不是整数时抛出。
;;
;; 示例
;; ----
;; (list-take '(1 2 3 4 5) 3) => '(1 2 3)
;; (list-take '(1 2 3 4 5) 0) => '()
;; (list-take '(1 2 3 4 5) 5) => '(1 2 3 4 5)
;; (list-take '(1 2 3) -1) => '()
;; (list-take '(1 2 3) 10) => '(1 2 3)

; 基本功能测试
(check (list-take '(1 2 3 4 5) 3) => '(1 2 3))
(check (list-take '(1 2 3 4 5) 0) => '())
(check (list-take '(1 2 3 4 5) 5) => '(1 2 3 4 5))

; 边界容忍测试（与take的主要区别）
(check (list-take '(1 2 3) -1) => '())
(check (list-take '(1 2 3) 10) => '(1 2 3))

; 空列表测试
(check (list-take '() 0) => '())
(check (list-take '() 5) => '())

; 错误处理测试
(check-catch 'type-error (list-take "not a list" 2))
(check-catch 'type-error (list-take '(1 2 3) "not a number"))

(check-report)
