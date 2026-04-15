(import (liii check))
(import (liii list))
(import (scheme base))
(check-set-mode! 'report-failed)
;; append
;; 创建新的列表，将0个或多个列表合并为一个新列表。
;; 
;; 语法
;; -----
;; (append list ...)
;; 
;; 参数
;; -----
;; list ... : list?
;; 任意数量的列表。如果没有参数，返回空列表；如果有一个参数，返回该参数本身；
;; 如果有多个参数，返回将除最后一个参数外所有参数的元素复制到新列表后，最后一个参数成为最后一个列表。
;; 
;; 返回值
;; ------
;; list?
;; 返回将所有参与列表合并的新列表。如果没有参数返回空列表，如果有多个参数则返回
;; deep-structure合并列表（即共享最后一个列表的结构）。
;; 
;; 说明
;; ----
;; 1. 对于单参数，append返回该列表本身
;; 2. 对于多参数，前面的list会被复制，最后一个list被共享
;; 3. 允许非列表的最后一个参数（形成点对结构），这种行为符合R7RS规范
;; 4. 对于空列表参数，append起到忽略空列表的作用
;; 5. 实际效率：除最后一个参数外，其他列表会被复制
;; 
;; 错误
;; ---
;; 该行无错误情况。所有参数可以是列表，或者最后一个参数可以是非列表。
;; append
;; 创建新的列表，将0个或多个列表合并为一个新列表。
;; 
;; 语法
;; -----
;; (append list ...)
;; 
;; 参数
;; -----
;; list ... : list?
;; 任意数量的列表。如果没有参数，返回空列表；如果有一个参数，返回该参数本身；
;; 如果有多个参数，返回将除最后一个参数外所有参数的元素复制到新列表后，最后一个参数成为最后一个列表。
;; 
;; 返回值
;; ------
;; list?
;; 返回将所有参与列表合并的新列表。如果没有参数返回空列表，如果有多个参数则返回
;; deep-structure合并列表（即共享最后一个列表的结构）。
;; 
;; 说明
;; ----
;; 1. 对于单参数，append返回该列表本身
;; 2. 对于多参数，前面的list会被复制，最后一个list被共享
;; 3. 允许非列表的最后一个参数（形成点对结构），这种行为符合R7RS规范
;; 4. 对于空列表参数，append起到忽略空列表的作用
;; 5. 实际效率：除最后一个参数外，其他列表会被复制
;; 
;; 错误
;; ---
;; 该行无错误情况。所有参数可以是列表，或者最后一个参数可以是非列表。
;; append
;; 创建新的列表，将0个或多个列表合并为一个新列表。
;; 
;; 语法
;; -----
;; (append list ...)
;; 
;; 参数
;; -----
;; list ... : list?
;; 任意数量的列表。如果没有参数，返回空列表；如果有一个参数，返回该参数本身；
;; 如果有多个参数，返回将除最后一个参数外所有参数的元素复制到新列表后，最后一个参数成为最后一个列表。
;; 
;; 返回值
;; ------
;; list?
;; 返回将所有参与列表合并的新列表。如果没有参数返回空列表，如果有多个参数则返回
;; deep-structure合并列表（即共享最后一个列表的结构）。
;; 
;; 说明
;; ----
;; 1. 对于单参数，append返回该列表本身
;; 2. 对于多参数，前面的list会被复制，最后一个list被共享
;; 3. 允许非列表的最后一个参数（形成点对结构），这种行为符合R7RS规范
;; 4. 对于空列表参数，append起到忽略空列表的作用
;; 5. 实际效率：除最后一个参数外，其他列表会被复制
;; 
;; 错误
;; ---
;; 该行无错误情况。所有参数可以是列表，或者最后一个参数可以是非列表。
;; 基础测试用例 - 空列表合并
(check (append '() '()) => '())
(check (append '() '() '()) => '())
(check (append '() '() '() '()) => '())
(check (append) => '())
;; 单参数测试（特化情况：直接返回原参数）
(check (append '(a b c)) => '(a b c))
(check (append '()) => '())
(check (append '(1 2 3 4 5))
  =>
  '(1 2 3 4 5)
) ;check
(check (append (list 1 2 3))
  =>
  (list 1 2 3)
) ;check
;; 双列表合并测试
(check (append '(a) '(b)) => '(a b))
(check (append '(a b) '(c d))
  =>
  '(a b c d)
) ;check
(check (append '(1 2) '(3 4 5))
  =>
  '(1 2 3 4 5)
) ;check
(check (append '(a b c) '(d e))
  =>
  '(a b c d e)
) ;check
(check (append '() '(a b c))
  =>
  '(a b c)
) ;check
(check (append '(a b c) '())
  =>
  '(a b c)
) ;check
;; 三列表合并测试
(check (append '(a) '(b) '(c))
  =>
  '(a b c)
) ;check
(check (append '(1 2) '(3 4) '(5 6))
  =>
  '(1 2 3 4 5 6)
) ;check
(check (append '(a b) '(c d e) '(f g))
  =>
  '(a b c d e f g)
) ;check
(check (append '(x y) '(n) '(a b c))
  =>
  '(x y n a b c)
) ;check
;; 多列表合并测试（复杂情况）
(check (append '(a) '(b) '(c) '(d))
  =>
  '(a b c d)
) ;check
(check (append '(1) '(2 3) '(4 5 6) '(7))
  =>
  '(1 2 3 4 5 6 7)
) ;check
(check (append '(a) '() '(b) '() '(c))
  =>
  '(a b c)
) ;check
;; 层次结构性质验证测试
(check (list? (append '(a) '())) => #t)
(check (list? (append '() '(a))) => #t)
(check (list? (append '(a b) '(c d)))
  =>
  #t
) ;check
(check (pair? (append '(a) 'b)) => #t)
;; 长度验证测试
(check (length (append '() '())) => 0)
(check (length (append '(a) '())) => 1)
(check (length (append '() '(b))) => 1)
(check (length (append '(a) '(b))) => 2)
(check (length (append '(a b) '(c d)))
  =>
  4
) ;check
(check (length (append '(1 2) '(3 4 5)))
  =>
  5
) ;check
(check (length (append '(1 2 3) '(4 5 6 7)))
  =>
  7
) ;check
;; 复杂嵌套结构测试
(check (append '((a b) (c d)) '(e f))
  =>
  '((a b) (c d) e f)
) ;check
(check (append '(a b) '((c d) (e f)))
  =>
  '(a b (c d) (e f))
) ;check
(check (append '((a) (b)) '((c) (d)))
  =>
  '((a) (b) (c) (d))
) ;check
(check (append '((a b) c) '(d (e f)))
  =>
  '((a b) c d (e f))
) ;check
;; 深层次结构测试
(check (append '(1 (2 (3))) '(((4) 5) 6))
  =>
  '(1 (2 (3)) ((4) 5) 6)
) ;check
(check (append '(a (b (c))) '(d (e (f))))
  =>
  '(a (b (c)) d (e (f)))
) ;check
(check (append '(() a ()) '(b))
  =>
  '(() a () b)
) ;check
;; 点对结构测试（核心特性）
(check (append '(a b) 'c) => '(a b . c))
(check (append '(a) 'b) => '(a . b))
(check (append '(a b c) 'd)
  =>
  '(a b c . d)
) ;check
(check (append '(a b) '(c d) 'e)
  =>
  '(a b c d . e)
) ;check
(check (append '() 'a) => 'a)
(check (append '(a) '() 'b) => '(a . b))
;; 复杂点对结构测试
(check (append '((a) b) 'c)
  =>
  '((a) b . c)
) ;check
(check (append '(1 2 3) '(a . b))
  =>
  '(1 2 3 a . b)
) ;check
(check (append '(a (b (c))) 'd)
  =>
  '(a (b (c)) . d)
) ;check
(check (append '(a b) '((c d) . e))
  =>
  '(a b (c d) . e)
) ;check
;; 反向连接测试（验证方向性）
(check (append '(z y x) '(c b a))
  =>
  '(z y x c b a)
) ;check
(check (append '(3 2 1) '(0 -1 -2))
  =>
  '(3 2 1 0 -1 -2)
) ;check
;; 大规模合并测试
(check (length (append '(1 2 3 4 5)))
  =>
  5
) ;check
(check (length (append '(1 2 3) '(4 5 6)))
  =>
  6
) ;check
(check (length (append '(1 2) '(3 4) '(5 6)))
  =>
  6
) ;check
;; 字符列表测试
(check (append '(#\t #\e) '(#\s #\t))
  =>
  '(#\t #\e #\s #\t)
) ;check
(check (append '(#\t) '(#\t) '(#\n))
  =>
  '(#\t #\t #\n)
) ;check
;; 字符串列表测试（复合数据类型）
(check (append '("hello") '("world"))
  =>
  '("hello" "world")
) ;check
(check (append '("a" "b") '("c" "d" "e"))
  =>
  '("a" "b" "c" "d" "e")
) ;check
(check (append '("中文" "测试")
         '("继续")
       ) ;append
  =>
  '("中文" "测试" "继续")
) ;check
;; 符号列表测试
(check (append '(a b c) '(d e f))
  =>
  '(a b c d e f)
) ;check
(check (append '(quote define)
         '(lambda procedure)
       ) ;append
  =>
  '(quote define lambda procedure)
) ;check
(check (append '(if cond) '(else))
  =>
  '(if cond else)
) ;check
;; 混合数据类型测试
(check (append '(1 "hello" #t) '(2 "world" #f))
  =>
  '(1 "hello" #t 2 "world" #f)
) ;check
(check (append '(a 1 "test" #\x) '(b 2 c))
  =>
  '(a 1 "test" #\x b 2 c)
) ;check
(check (append '(1.0 2.5) '(3.5 4.0))
  =>
  '(1.0 2.5 3.5 4.0)
) ;check
;; 数字列表测试
(check (append '(1 2 3 4) '(5 6 7 8))
  =>
  '(1 2 3 4 5 6 7 8)
) ;check
(check (append '(1 2 3) '(a b c))
  =>
  '(1 2 3 a b c)
) ;check
;; boolean列表测试
(check (append '(#t #f) '(#t #f))
  =>
  '(#t #f #t #f)
) ;check
(check (append '(#t) '(#f #t))
  =>
  '(#t #f #t)
) ;check
;; 验证使用list构造函数
(check (append (list 1 2) (list 3 4))
  =>
  '(1 2 3 4)
) ;check
(check (append (list 'a 'b) (list 'c 'd))
  =>
  '(a b c d)
) ;check
(check (append (list) (list 'x 'y))
  =>
  '(x y)
) ;check
;; 边界测试
(check (append '() '() '(a) '() '(b))
  =>
  '(a b)
) ;check
(check (append '(a) '() '(b) '() '(c))
  =>
  '(a b c)
) ;check
(check (append '(a) '(b) '() '(c) '(d))
  =>
  '(a b c d)
) ;check
;; 链式操作验证测试
(check (append (append '(1) '(2)) '(3))
  =>
  '(1 2 3)
) ;check
(check (append '(1) (append '(2) '(3)))
  =>
  '(1 2 3)
) ;check
(check (append (append '(a b) '(c)) '(d e))
  =>
  '(a b c d e)
) ;check
;; 结构对等性验证（通过elements检查）
(check (equal? (append '(1 2 3) '(4 5))
         '(1 2 3 4 5)
       ) ;equal?
  =>
  #t
) ;check
(check (equal? (append '(a) '(b) '(c d))
         '(a b c d)
       ) ;equal?
  =>
  #t
) ;check
(check (equal? (append '() '(first second) '())
         '(first second)
       ) ;equal?
  =>
  #t
) ;check
;; 函数结果作为append参数测试
(check (append (map (lambda (x) (* x 2)) '(1 2 3))
         '(7 8 9)
       ) ;append
  =>
  '(2 4 6 7 8 9)
) ;check
(check (append (filter (lambda (x) (> x 2)) '(1 2 3 4))
         '(5 6 7)
       ) ;append
  =>
  '(3 4 5 6 7)
) ;check
;; 验证结构共享性（非复制性测试）
(let ((lst-last '(last list)))
  (let ((result (append '(copy list) lst-last))
       ) ;
    (check (equal? result '(copy list last list))
      =>
      #t
    ) ;check
    (check (eq? (cdr (cdr result)) lst-last)
      =>
      #t
    ) ;check
  ) ;let
) ;let
;; 空列表和复杂结构组合测试
(check (append '() '(a (b c) d) '())
  =>
  '(a (b c) d)
) ;check
(check (append '((a b) c) '() '(d))
  =>
  '((a b) c d)
) ;check
;; 深度验证测试（确保append正确性）
(check (length (append '(a b c d) '(e f g h)))
  =>
  8
) ;check
(check (length (append '(1 2) '(3 4) '(5 6) '(7 8))
       ) ;length
  =>
  8
) ;check
(check (list-ref (append '(1 2 3) '(4 5 6)) 5)
  =>
  6
) ;check
(check (list-ref (append '(1) '(2 3 4)) 3)
  =>
  4
) ;check
(check-report)
