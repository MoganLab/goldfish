(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; take 函数测试
;;
;; 从列表开头提取指定数量的元素。
;;
;; 语法
;; -----
;; (take list k)
;;
;; 参数
;; -----
;; list : list?
;; 源列表，从中提取元素。
;;
;; k : integer?
;; 要提取的元素数量，必须是非负整数且不超过列表长度。
;;
;; 返回值
;; ------
;; list
;; 包含指定数量元素的新列表，从原列表的开头开始计数。
;;
;; 注意
;; -----
;; take函数会创建新的列表结构，原列表不会被修改。当k等于列表长度时，返回完整列表的拷贝。
;; 对于点结尾的列表，行为与proper列表相同，直到遇到dot才停止。
;;
;; 示例
;; -----
;; (take '(1 2 3 4) 2) => '(1 2)
;; (take '(a b c) 0) => '()
;; (take '(1 (2 3) 4) 2) => '(1 (2 3))
;; (take '((a b) (c d)) 1) => '((a b))
;;
;; 边界条件
;; -----
;; 空列表返回空列表
;; (take '() 0) => '()
;;
;; 错误处理
;; -----
;; wrong-type-arg 当list不是列表或k不是整数类型时
;; out-of-range 当k超过列表长度或k为负数时


(check (take '(1 2 3 4) 3) => '(1 2 3))
(check (take '(1 2 3 4) 4) => '(1 2 3 4))
(check (take '(1 2 3 . 4) 3) => '(1 2 3))


(check-catch 'wrong-type-arg (take '(1 2 3 4) 5))
(check-catch 'wrong-type-arg (take '(1 2 3 . 4) 4))


(check (take '() 0) => '())
(check (take '(a) 1) => '(a))
(check (take '(a) 0) => '())
(check (take '((a) (b c) d) 2) => '((a) (b c)))
(check (take '(1 2 3 4 5 6) 3) => '(1 2 3))


(check (take (drop '(1 2 3 4 5) 1) 3) => '(2 3 4))
(check (take (take '(1 2 3 4 5) 4) 2) => '(1 2))


(check (take (iota 10) 5) => '(0 1 2 3 4))


(check-catch 'wrong-type-arg (take '(1 2 3) -1))
(check-catch 'wrong-type-arg (take "not a list" 2))
(check-catch 'wrong-type-arg (take '(1 2 3) "not a number"))


;; take 总是创建新的列表结构，即使取走全部元素也不共享节点
(let ((l (list 1 2 3)))
  (check (eq? (take l 3) l) => #f)
  (check (take l 3) => '(1 2 3))
  ;; 修改结果不影响原列表
  (let ((r (take l 2)))
    (set-car! r 99)
    (check l => '(1 2 3))
  ) ;let
) ;let


;; 大列表 + GC 压力回归测试
(let ((l (iota 10000)))
  (check (take l 5000) => (iota 5000))
  (check (length (take l 10000)) => 10000)
  (check l => (iota 10000))
) ;let

(do ((i 0 (+ i 1)))
  ((= i 1000))
  (take (iota 100) 50)
) ;do


(check-report)
