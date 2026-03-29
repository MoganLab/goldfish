(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; take-right 函数测试
;;
;; 从列表尾部提取指定数量的元素。
;;
;; 语法
;; -----
;; (take-right list k)
;;
;; 参数
;; -----
;; list : list?
;; 源列表，从中从尾部提取元素。
;;
;; k : integer?
;; 要从尾部提取的元素数量，必须是非负整数且不超过列表长度。
;;
;; 返回值
;; ------
;; list
;; 包含从尾部开始指定数量元素的新列表。
;;
;; 注意
;; -----
;; take-right函数与take功能相反，从列表右侧（尾部）提取元素。
;;
;; 示例
;; -----
;; (take-right '(1 2 3 4) 2) => '(3 4)
;; (take-right '(a b c) 1) => '(c)
;; (take-right '((a b) (c d) (e f)) 2) => '((c d) (e f))
;;
;; 边界条件
;; -----
;; 空列表返回空列表
;; (take-right '() 0) => '()
;;
;; 错误处理
;; -----
;; out-of-range 当k超过列表长度或k为负数时
;; wrong-type-arg 当list不是列表或k不是整数类型时

(check (take-right '(1 2 3 4) 3) => '(2 3 4))
(check (take-right '(1 2 3 4) 4) => '(1 2 3 4))
(check (take-right '(1 2 3 . 4) 3) => '(1 2 3 . 4))

; 更多边界条件测试
(check (take-right '() 0) => '())
(check (take-right '(a) 1) => '(a))
(check (take-right '(a) 0) => '())
(check (take-right '((a) (b c) d) 2) => '((b c) d))
(check (take-right '(1 2 3 4 5 6) 3) => '(4 5 6))

; 链式操作测试
(check (take-right (drop '(1 2 3 4 5) 1) 3) => '(3 4 5))
(check (take-right (take '(1 2 3 4 5) 4) 2) => '(3 4))

; 大列表测试
(check (take-right (iota 10) 5) => '(5 6 7 8 9))

; 边缘情况测试
(check (take-right '(1) 1) => '(1))
(check (take-right '(1 2) 1) => '(2))

; 错误条件测试
(check-catch 'out-of-range (take-right '(1 2 3 4) 5))
(check-catch 'out-of-range (take-right '(1 2 3 . 4) 4))
(check-catch 'out-of-range (take-right '(1 2 3) -1))
(check-catch 'wrong-type-arg (take-right "not a list" 2))
(check-catch 'wrong-type-arg (take-right '(1 2 3) "not a number"))

(check-report)
