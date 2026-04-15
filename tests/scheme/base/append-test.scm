(import (liii check))
(import (liii list))
(import (scheme base))
(check-set-mode! 'report-failed)
;; 基础测试用例 - 空列表合并
(check (append '() '()) => '())
(check (append '() '() '()) => '())
(check (append '() '() '() '()) => '())
(check (append) => '())
;; 单参数测试（特化情况：直接返回原参数）
(check (append '(a b c)) => '(a b c))
(check (append '()) => '())
(check (append '(1 2 3 4 5)) => '(1 2 3 4 5))
(check (append (list 1 2 3)) => (list 1 2 3))
;; 双列表合并测试
(check (append '(a) '(b)) => '(a b))
(check (append '(a b) '(c d)) => '(a b c d))
(check (append '(1 2) '(3 4 5)) => '(1 2 3 4 5))
(check (append '(a b c) '(d e)) => '(a b c d e))
(check (append '() '(a b c)) => '(a b c))
(check (append '(a b c) '()) => '(a b c))
;; 三列表合并测试
(check (append '(a) '(b) '(c)) => '(a b c))
(check (append '(1 2) '(3 4) '(5 6)) => '(1 2 3 4 5 6))
(check (append '(a b) '(c d e) '(f g)) => '(a b c d e f g))
(check (append '(x y) '(n) '(a b c)) => '(x y n a b c))
;; 多列表合并测试（复杂情况）
(check (append '(a) '(b) '(c) '(d)) => '(a b c d))
(check (append '(1) '(2 3) '(4 5 6) '(7))
  =>
  '(1 2 3 4 5 6 7)
) ;check
(check (append '(a) '() '(b) '() '(c)) => '(a b c))
;; 层次结构性质验证测试
(check (list? (append '(a) '())) => #t)
(check (list? (append '() '(a))) => #t)
(check (list? (append '(a b) '(c d))) => #t)
(check (pair? (append '(a) 'b)) => #t)
;; 长度验证测试
(check (length (append '() '())) => 0)
(check (length (append '(a) '())) => 1)
(check (length (append '() '(b))) => 1)
(check (length (append '(a) '(b))) => 2)
(check (length (append '(a b) '(c d))) => 4)
(check (length (append '(1 2) '(3 4 5))) => 5)
(check (length (append '(1 2 3) '(4 5 6 7))) => 7)
;; 复杂嵌套结构测试
(check (append '((a b) (c d)) '(e f)) => '((a b) (c d) e f))
(check (append '(a b) '((c d) (e f))) => '(a b (c d) (e f)))
(check (append '((a) (b)) '((c) (d))) => '((a) (b) (c) (d)))
(check (append '((a b) c) '(d (e f))) => '((a b) c d (e f)))
;; 深层次结构测试
(check (append '(1 (2 (3))) '(((4) 5) 6))
  =>
  '(1 (2 (3)) ((4) 5) 6)
) ;check
(check (append '(a (b (c))) '(d (e (f))))
  =>
  '(a (b (c)) d (e (f)))
) ;check
(check (append '(() a ()) '(b)) => '(() a () b))
;; 点对结构测试（核心特性）
(check (append '(a b) 'c) => '(a b . c))
(check (append '(a) 'b) => '(a . b))
(check (append '(a b c) 'd) => '(a b c . d))
(check (append '(a b) '(c d) 'e) => '(a b c d . e))
(check (append '() 'a) => 'a)
(check (append '(a) '() 'b) => '(a . b))
;; 复杂点对结构测试
(check (append '((a) b) 'c) => '((a) b . c))
(check (append '(1 2 3) '(a . b)) => '(1 2 3 a . b))
(check (append '(a (b (c))) 'd) => '(a (b (c)) . d))
(check (append '(a b) '((c d) . e)) => '(a b (c d) . e))
;; 反向连接测试（验证方向性）
(check (append '(z y x) '(c b a)) => '(z y x c b a))
(check (append '(3 2 1) '(0 -1 -2)) => '(3 2 1 0 -1 -2))
;; 大规模合并测试
(check (length (append '(1 2 3 4 5))) => 5)
(check (length (append '(1 2 3) '(4 5 6))) => 6)
(check (length (append '(1 2) '(3 4) '(5 6))) => 6)
;; 字符列表测试
(check (append '(#\t #\e) '(#\s #\t)) => '(#\t #\e #\s #\t))
(check (append '(#\t) '(#\t) '(#\n)) => '(#\t #\t #\n))
;; 字符串列表测试（复合数据类型）
(check (append '("hello") '("world")) => '("hello" "world"))
(check (append '("a" "b") '("c" "d" "e"))
  =>
  '("a" "b" "c" "d" "e")
) ;check
(check (append '("中文" "测试") '("继续"))
  =>
  '("中文" "测试" "继续")
) ;check
;; 符号列表测试
(check (append '(a b c) '(d e f)) => '(a b c d e f))
(check (append '(quote define) '(lambda procedure))
  =>
  '(quote define lambda procedure)
) ;check
(check (append '(if cond) '(else)) => '(if cond else))
;; 混合数据类型测试
(check (append '(1 "hello" #t) '(2 "world" #f))
  =>
  '(1 "hello" #t 2 "world" #f)
) ;check
(check (append '(a 1 "test" #\x) '(b 2 c))
  =>
  '(a 1 "test" #\x b 2 c)
) ;check
(check (append '(1.0 2.5) '(3.5 4.0)) => '(1.0 2.5 3.5 4.0))
;; 数字列表测试
(check (append '(1 2 3 4) '(5 6 7 8)) => '(1 2 3 4 5 6 7 8))
(check (append '(1 2 3) '(a b c)) => '(1 2 3 a b c))
;; boolean列表测试
(check (append '(#t #f) '(#t #f)) => '(#t #f #t #f))
(check (append '(#t) '(#f #t)) => '(#t #f #t))
;; 验证使用list构造函数
(check (append (list 1 2) (list 3 4)) => '(1 2 3 4))
(check (append (list 'a 'b) (list 'c 'd)) => '(a b c d))
(check (append (list) (list 'x 'y)) => '(x y))
;; 边界测试
(check (append '() '() '(a) '() '(b)) => '(a b))
(check (append '(a) '() '(b) '() '(c)) => '(a b c))
(check (append '(a) '(b) '() '(c) '(d)) => '(a b c d))
;; 链式操作验证测试
(check (append (append '(1) '(2)) '(3)) => '(1 2 3))
(check (append '(1) (append '(2) '(3))) => '(1 2 3))
(check (append (append '(a b) '(c)) '(d e)) => '(a b c d e))
;; 结构对等性验证（通过elements检查）
(check (equal? (append '(1 2 3) '(4 5)) '(1 2 3 4 5)) => #t)
(check (equal? (append '(a) '(b) '(c d)) '(a b c d)) => #t)
(check (equal? (append '() '(first second) '()) '(first second))
  =>
  #t
) ;check
;; 函数结果作为append参数测试
(check (append (map (lambda (x) (* x 2)) '(1 2 3)) '(7 8 9))
  =>
  '(2 4 6 7 8 9)
) ;check
(check (append (filter (lambda (x) (> x 2)) '(1 2 3 4)) '(5 6 7))
  =>
  '(3 4 5 6 7)
) ;check
;; 验证结构共享性（非复制性测试）
(let ((lst-last '(last list)))
  (let ((result (append '(copy list) lst-last)))
    (check (equal? result '(copy list last list)) => #t)
    (check (eq? (cdr (cdr result)) lst-last) => #t)
  ) ;let
) ;let
;; 空列表和复杂结构组合测试
(check (append '() '(a (b c) d) '()) => '(a (b c) d))
(check (append '((a b) c) '() '(d)) => '((a b) c d))
;; 深度验证测试（确保append正确性）
(check (length (append '(a b c d) '(e f g h))) => 8)
(check (length (append '(1 2) '(3 4) '(5 6) '(7 8))) => 8)
(check (list-ref (append '(1 2 3) '(4 5 6)) 5) => 6)
(check (list-ref (append '(1) '(2 3 4)) 3) => 4)
(check-report)