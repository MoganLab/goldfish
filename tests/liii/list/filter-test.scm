(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; filter 函数测试
;;
;; 按照SRFI-1规范过滤列表中满足谓词的元素。
;;
;; 语法
;; ----
;; (filter pred list)
;;
;; 参数
;; ----
;; pred : procedure?
;; 谓词函数，接受单个参数并返回布尔值。
;;
;; list : list?
;; 要过滤的列表。
;;
;; 返回值
;; ------
;; list
;; 包含所有满足谓词条件的元素的新列表，保持原有顺序。
;;
;; 说明
;; ----
;; filter函数遍历输入列表并对每个元素应用谓词函数，收集所有使谓词返回#t的元素，
;; 保持它们在原列表中的相对顺序。这是一个纯函数操作，不会修改原始列表。
;;
;; 使用场景
;; --------
;; - 从数据集合中提取满足特定条件的子集
;; - 数据清洗和预处理
;; - 基于条件的数据筛选
;; - 集合操作中的子集提取
;;
;; 边界条件
;; --------
;; - 空列表返回空列表
;; - 如果没有元素满足条件返回空列表
;; - 如果所有元素都满足条件返回原列表的拷贝
;; - 可以保持原始列表的顺序
;;
;; 注意事项
;; --------
;; - pred必须是接受单个参数的函数
;; - filter是引用透明的纯函数操作
;; - 对于大列表可能会创建大量临时对象
;;
;; 示例
;; ----
;; 基础用法：
;; (filter even? '(1 2 3 4 5 6)) => '(2 4 6)
;; (filter (lambda (x) (> x 3)) '(1 2 3 4 5)) => '(4 5)
;; (filter symbol? '(a 1 b 2 c 3)) => '(a b c)
;;
;; 边界情况：
;; (filter even? '()) => '()
;; (filter (lambda (x) #f) '(1 2 3)) => '()
;; (filter (lambda (x) #t) '(1 2 3)) => '(1 2 3)
;; 复杂数据类型：
;; (filter list? '(1 (2 3) 4 (5 6))) => '((2 3) (5 6))
;; (filter (lambda (x) (string? x)) '("a" 1 "b" 2)) => '("a" "b")


(check (filter even? '(-2 -1 0 1 2))
  =>
  '(-2 0 2)
) ;check
(check (filter even? '(1 2 3 4 5 6))
  =>
  '(2 4 6)
) ;check
(check (filter odd? '(1 2 3 4 5 6))
  =>
  '(1 3 5)
) ;check
(check (filter positive? '(-2 -1 0 1 2))
  =>
  '(1 2)
) ;check
(check (filter negative? '(-2 -1 0 1 2))
  =>
  '(-2 -1)
) ;check


(check (filter even? '()) => '())
(check (filter (lambda (x) #f) '(1 2 3))
  =>
  '()
) ;check
(check (filter (lambda (x) #t) '(1 2 3))
  =>
  '(1 2 3)
) ;check
(check (filter (lambda (x) (> x 100)) '(1 2 3))
  =>
  '()
) ;check


(check (filter symbol? '(a 1 b 2 c 3))
  =>
  '(a b c)
) ;check
(check (filter string?
         '("hello" 42 "world" 3.14)
       ) ;filter
  =>
  '("hello" "world")
) ;check
(check (filter list? '(1 (2 3) 4 (5 (6))))
  =>
  '((2 3) (5 (6)))
) ;check
(check (filter boolean? '(#t #f 1 "a" #t))
  =>
  '(#t #f #t)
) ;check


(check (filter (lambda (x)
                 (and (list? x) (not (null? x)))
               ) ;lambda
         '(() (a) b (c d) ())
       ) ;filter
  =>
  '((a) (c d))
) ;check
(check (filter (lambda (x)
                 (and (list? x) (> (length x) 1))
               ) ;lambda
         '((a) (b c) (d) (e f g))
       ) ;filter
  =>
  '((b c) (e f g))
) ;check


(check (filter (lambda (x) (and x (> x 5)))
         '(3 7 4 9 2 8)
       ) ;filter
  =>
  '(7 9 8)
) ;check
(check (filter (lambda (x) (> (string-length x) 3))
         '("a" "hello" "xyz" "world")
       ) ;filter
  =>
  '("hello" "world")
) ;check


(let ((numbers (iota 100)))
  (check (filter (lambda (x) (= (modulo x 10) 0))
           numbers
         ) ;filter
    =>
    '(0 10 20 30 40 50 60 70 80 90)
  ) ;check
) ;let


(let ((large-list (make-list 1000 5)))
  (check (length (filter (lambda (x) #t) large-list)
         ) ;length
    =>
    1000
  ) ;check
  (check (length (filter (lambda (x) (> x 0)) large-list)
         ) ;length
    =>
    1000
  ) ;check
  (check (length (filter (lambda (x) (> x 5)) large-list)
         ) ;length
    =>
    0
  ) ;check
) ;let


(check-report)
