(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; 基础测试：普通列表索引访问
(check (list-ref '(a b c d e) 0) => 'a)
(check (list-ref '(a b c d e) 1) => 'b)
(check (list-ref '(a b c d e) 4) => 'e)
(check (list-ref '(1 2 3 4 5) 0) => 1)
(check (list-ref '(1 2 3 4 5) 2) => 3)
(check (list-ref '(1 2 3 4 5) 4) => 5)
;; 边界值测试：索引0、中间值、最大值
(check (list-ref '(single) 0)
  =>
  'single
) ;check
(check (list-ref '(x y z) 1) => 'y)
(check (list-ref '(first last) 1)
  =>
  'last
) ;check
;; 复杂数据类型测试：包含各种类型元素
(check (list-ref '("string" 42 #t symbol) 0)
  =>
  "string"
) ;check
(check (list-ref '("string" 42 #t symbol) 1)
  =>
  42
) ;check
(check (list-ref '("string" 42 #t symbol) 2)
  =>
  #t
) ;check
(check (list-ref '("string" 42 #t symbol) 3)
  =>
  'symbol
) ;check
;; 嵌套列表测试：访问嵌套结构
(check (list-ref '((1 2 3) (4 5 6) (7 8 9)) 0)
  =>
  '(1 2 3)
) ;check
(check (list-ref '("hel" "lo" "wo" "rld") 2)
  =>
  "wo"
) ;check
(check (list-ref '(((a b) c d) e f) 0)
  =>
  '((a b) c d)
) ;check
;; 测试点对结构：简单点对
(check (list-ref '(a . b) 0) => 'a)
(check (list-ref (cons 'a 'b) 0) => 'a)
;; improper列表测试：包含点对结构的列表
(check (list-ref '(a b c . d) 0) => 'a)
(check (list-ref '(a b c . d) 1) => 'b)
(check (list-ref '(a b c . d) 2) => 'c)
;; 复杂嵌套结构测试：深层嵌套
(check (list-ref '((a b) (c d) (e f)) 1)
  =>
  '(c d)
) ;check
(check (list-ref '((1 2) 3 4 5) 0)
  =>
  '(1 2)
) ;check
(check (list-ref '((1 2) 3 4 5) 3) => 5)
;; 简单点对重新测试
(check (list-ref (cons '(1 2) '(3 4)) 1)
  =>
  3
) ;check
;; 基础三元列表
(check (list-ref '(a b c) 0) => 'a)
(check (list-ref '(a b c) 1) => 'b)
(check (list-ref '(a b c) 2) => 'c)
;; 错误情况测试
(check-catch 'wrong-type-arg
  (list-ref '() 0)
) ;check-catch
(check-catch 'wrong-type-arg
  (list-ref 123 0)
) ;check-catch
(check-catch 'wrong-type-arg
  (list-ref "string" 1)
) ;check-catch
(check-catch 'wrong-type-arg
  (list-ref #t 1)
) ;check-catch
;; 索引越界测试
(check-catch 'out-of-range
  (list-ref '(a b c) -1)
) ;check-catch
(check-catch 'out-of-range
  (list-ref '(a b c) 3)
) ;check-catch
(check-catch 'out-of-range
  (list-ref '(x) 1)
) ;check-catch
(check-catch 'out-of-range
  (list-ref '(a b c d e) 5)
) ;check-catch
(check-catch 'out-of-range
  (list-ref '(single) 2)
) ;check-catch
;; 构造器函数创建的列表测试
(check (list-ref (list 1 2 3 4) 2) => 3)
(check (list-ref (cons 1 (cons 2 (cons 3 '())))
         1
       ) ;list-ref
  =>
  2
) ;check
(check (list-ref (append '(1 2) '(3 4 5)) 3)
  =>
  4
) ;check
;; 附加的列表操作场景测试
(check (list-ref '(apple banana cherry date elderberry)
         2
       ) ;list-ref
  =>
  'cherry
) ;check
(check (list-ref (list 'symbol 42 #t "string" 3.14)
         3
       ) ;list-ref
  =>
  "string"
) ;check
(check-report)