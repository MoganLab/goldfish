(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; drop 函数测试
;;
;; 从列表开头删除指定数量的元素。
;;
;; 语法
;; ----
;; (drop list k)
;;
;; 参数
;; ----
;; list : list?
;; 源列表，从中删除元素。
;;
;; k : integer?
;; 要从开头删除的元素数量，必须是非负整数且不超过列表长度。
;;
;; 返回值
;; ------
;; list
;; 删除k个元素后的新列表。当k等于列表长度时，返回空列表。
;;
;; 说明
;; ----
;; drop函数与take函数功能相反，从列表前端删除元素而不是提取。
;; 对于proper list，返回的是剩余部分的列表；
;; 对于dotted list，如果k等于列表长度减一，返回的是最后的非列表元素。
;;
;; drop和take互为补操作：(take lst k) + (drop lst k) = lst
;;
;; 示例
;; ----
;; (drop '(1 2 3 4) 2) => '(3 4)
;; (drop '(a b c) 1) => '(b c)
;; (drop '(1 (2 3) 4) 2) => '(4)
;; (drop '() 0) => '()
;; (drop '(a) 0) => '(a)
;;
;; 与dotted list交互:
;; (drop '(1 2 . 3) 1) => '(2 . 3)
;; (drop '(1 2 . 3) 2) => 3
;;
;; 边界条件
;; --------
;; - 空列表：返回空列表
;; - 零个元素删除：返回原列表
;; - 删除所有元素：返回空列表
;;
;; 错误处理
;; --------
;; - out-of-range：当k超过列表长度时
;; - wrong-type-arg：当list不是列表或k不是整数类型时


(check (drop '(1 2 3 4) 2) => '(3 4))
(check (drop '(1 2 3 4) 4) => '())
(check (drop '(1 2 3 . 4) 3) => 4)


(check (drop '(1 2 3 4 5) 0)
  =>
  '(1 2 3 4 5)
) ;check
(check (drop '(1 2 3 4 5) 1)
  =>
  '(2 3 4 5)
) ;check
(check (drop '(1 2 3 4 5) 3) => '(4 5))
(check (drop '(1 2 3 4 5) 5) => '())


(check (drop '() 0) => '())


(check (drop '(a) 0) => '(a))
(check (drop '(a) 1) => '())


(check (drop '((a b) (c d) (e f)) 1)
  =>
  '((c d) (e f))
) ;check
(check (drop '((a b) (c d) (e f)) 2)
  =>
  '((e f))
) ;check
(check (drop '((a b) (c d) (e f)) 3)
  =>
  '()
) ;check


(check (drop '(1 2 . 3) 0)
  =>
  '(1 2 . 3)
) ;check
(check (drop '(1 2 . 3) 1) => '(2 . 3))
(check (drop '(1 2 . 3) 2) => 3)
(check (drop '(a b c . d) 1)
  =>
  '(b c . d)
) ;check
(check (drop '(a b c . d) 2)
  =>
  '(c . d)
) ;check
(check (drop '(a b c . d) 3) => 'd)


(check (drop (drop '(1 2 3 4 5) 1) 2)
  =>
  '(4 5)
) ;check
(check (drop (take '(1 2 3 4 5) 4) 2)
  =>
  '(3 4)
) ;check
(check (take (drop '(1 2 3 4 5) 2) 2)
  =>
  '(3 4)
) ;check


(let ((lst (iota 10)))
  (check (drop lst 0)
    =>
    '(0 1 2 3 4 5 6 7 8 9)
  ) ;check
  (check (drop lst 5) => '(5 6 7 8 9))
  (check (drop lst 10) => '())
) ;let


(let ((lst '(1 2 3 4 5 6 7 8 9 10)))
  (define (symmetry-test lst k)
    (let ((take-part (take lst k))
          (drop-part (drop lst k))
         ) ;
      (append take-part drop-part)
    ) ;let
  ) ;define

  (check (symmetry-test lst 0) => lst)
  (check (symmetry-test lst 3) => lst)
  (check (symmetry-test lst 10) => lst)
  (check (symmetry-test lst 5) => lst)
) ;let


(check-catch 'out-of-range
  (drop '(1 2 3 4) 5)
) ;check-catch
(check-catch 'out-of-range
  (drop '(1 2 3 . 4) 4)
) ;check-catch
(check-catch 'out-of-range
  (drop '(1 2 3 4) -1)
) ;check-catch
(check-catch 'wrong-type-arg
  (drop "not a list" 2)
) ;check-catch
(check-catch 'wrong-type-arg
  (drop '(1 2 3) "not a number")
) ;check-catch


(check-report)
