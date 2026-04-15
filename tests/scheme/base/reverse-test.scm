(import (liii check))
(import (liii list))
(import (scheme base))
(check-set-mode! 'report-failed)
;; reverse
;; 返回一个新列表，包含原始列表中的元素但顺序相反。
;; 
;; 语法
;; ----
;; (reverse list)
;; 
;; 参数
;; ----
;; list : list?
;;     要反转的列表。
;; 
;; 返回值
;; ------
;; list?
;;     包含原始列表元素的反转顺序新列表。
;; 
;; 说明
;; ----
;; 1. 返回一个新的列表，元素顺序与原始列表相反
;; 2. 适用于所有类型的列表：空列表、非空列表、嵌套列表
;; 3. 不会改变原始列表的内容
;; 4. 对列表中的元素类型没有限制
;; 5. 当参数是非列表时，行为依赖于具体实现
;; 
;; 错误处理
;; --------
;; 依赖具体实现行为
;; reverse
;; 返回一个新列表，包含原始列表中的元素但顺序相反。
;; 
;; 语法
;; ----
;; (reverse list)
;; 
;; 参数
;; ----
;; list : list?
;;     要反转的列表。
;; 
;; 返回值
;; ------
;; list?
;;     包含原始列表元素的反转顺序新列表。
;; 
;; 说明
;; ----
;; 1. 返回一个新的列表，元素顺序与原始列表相反
;; 2. 适用于所有类型的列表：空列表、非空列表、嵌套列表
;; 3. 不会改变原始列表的内容
;; 4. 对列表中的元素类型没有限制
;; 5. 当参数是非列表时，行为依赖于具体实现
;; 
;; 错误处理
;; --------
;; 依赖具体实现行为
;; reverse
;; 返回一个新列表，包含原始列表中的元素但顺序相反。
;; 
;; 语法
;; ----
;; (reverse list)
;; 
;; 参数
;; ----
;; list : list?
;;     要反转的列表。
;; 
;; 返回值
;; ------
;; list?
;;     包含原始列表元素的反转顺序新列表。
;; 
;; 说明
;; ----
;; 1. 返回一个新的列表，元素顺序与原始列表相反
;; 2. 适用于所有类型的列表：空列表、非空列表、嵌套列表
;; 3. 不会改变原始列表的内容
;; 4. 对列表中的元素类型没有限制
;; 5. 当参数是非列表时，行为依赖于具体实现
;; 
;; 错误处理
;; --------
;; 依赖具体实现行为
(check (reverse '()) => '())
(check (reverse '(a)) => '(a))
(check (reverse '(a b)) => '(b a))
(check (reverse '(a b c)) => '(c b a))
(check (reverse '(1 2 3 4 5))
  =>
  '(5 4 3 2 1)
) ;check
(check (reverse '(x y z)) => '(z y x))
;; 测试空列表
(check (reverse '()) => '())
(check (equal? (reverse '()) '()) => #t)
;; 测试单元素列表
(check (reverse '(a)) => '(a))
(check (reverse '(1)) => '(1))
(check (reverse '("hello"))
  =>
  '("hello")
) ;check
(check (reverse '(#t)) => '(#t))
;; 测试双元素列表
(check (reverse '(a b)) => '(b a))
(check (reverse '(1 2)) => '(2 1))
(check (reverse '("first" "second"))
  =>
  '("second" "first")
) ;check
(check (reverse '(#t #f)) => '(#f #t))
;; 测试三元素列表
(check (reverse '(a b c)) => '(c b a))
(check (reverse '(1 2 3)) => '(3 2 1))
(check (reverse '("A" "B" "C"))
  =>
  '("C" "B" "A")
) ;check
;; 测试长列表
(check (reverse '(1 2 3 4 5 6 7 8 9 10))
  =>
  '(10 9 8 7 6 5 4 3 2 1)
) ;check
(check (reverse '(a b c d e f g h i j))
  =>
  '(j i h g f e d c b a)
) ;check
;; 测试嵌套列表
(check (reverse '((a b) (c d) (e f)))
  =>
  '((e f) (c d) (a b))
) ;check
(check (reverse '((1 (2 3)) 4 (5 6)))
  =>
  '((5 6) 4 (1 (2 3)))
) ;check
(check (reverse '("apple" ("banana" "cherry") "date")
       ) ;reverse
  =>
  '("date" ("banana" "cherry") "apple")
) ;check
;; 测试混合类型列表
(check (reverse '(1 "two" #t 4.5 symbol))
  =>
  '(symbol 4.5 #t "two" 1)
) ;check
(check (reverse '(#\newline "string" 42 #t))
  =>
  '(#t 42 "string" #\newline)
) ;check
;; 测试特殊元素
(check (reverse '(#\tab #\newline #\space))
  =>
  '(#\space #\newline #\tab)
) ;check
;; 测试由构造函数创建的列表
(check (reverse (list 1 2 3 4 5))
  =>
  '(5 4 3 2 1)
) ;check
(check (reverse (cons 'a (cons 'b (cons 'c '())))
       ) ;reverse
  =>
  '(c b a)
) ;check
;; 测试复杂嵌套结构
(check (reverse '((a . b) (c . d) (e . f)))
  =>
  '((e . f) (c . d) (a . b))
) ;check
(check (reverse '(a (b) (c (d))))
  =>
  '((c (d)) (b) a)
) ;check
;; 测试列表操作结果
(let ((lst (list 1 2 3 4 5)))
  (check (reverse (reverse lst)) => lst)
) ;let
;; 测试列表结构保持
(check (reverse '(a b c)) => '(c b a))
;; 测试包含空列表的情况
(check (reverse '(() (a) ()))
  =>
  '(() (a) ())
) ;check
;; 验证列表反转的正确性
(check (equal? (reverse '(1 2 3)) '(3 2 1))
  =>
  #t
) ;check
(check (equal? (reverse '("hello" "world"))
         '("world" "hello")
       ) ;equal?
  =>
  #t
) ;check
;; 测试边界情况
(check (reverse (list)) => '())
(check (reverse (cons 'a '())) => '(a))
;; 特殊字符测试
(check (reverse '((symbol "\\u4E2D\\u6587") (symbol "\\u5B57\\u7B26"))
       ) ;reverse
  =>
  '((symbol "\\u5B57\\u7B26") (symbol "\\u4E2D\\u6587"))
) ;check
(check (reverse '("foo" "bar" "baz" "" "qux"))
  =>
  '("qux" "" "baz" "bar" "foo")
) ;check
;; 长列表测试
(check (reverse (map (lambda (x) (* x x)) '(1 2 3 4 5))
       ) ;reverse
  =>
  '(25 16 9 4 1)
) ;check
(check (reverse (filter even? '(1 2 3 4 5 6 7 8))
       ) ;reverse
  =>
  '(8 6 4 2)
) ;check
(check-report)
