(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; list-tail 基本功能测试
(check (list-tail '(a b c d) 0)
  =>
  '(a b c d)
) ;check
(check (list-tail '(a b c d) 1)
  =>
  '(b c d)
) ;check
(check (list-tail '(a b c d) 2)
  =>
  '(c d)
) ;check
(check (list-tail '(a b c d) 3) => '(d))
(check (list-tail '(a b c d) 4) => '())
;; 边界值测试
(check (list-tail '(single) 0)
  =>
  '(single)
) ;check
(check (list-tail '(single) 1) => '())
(check (list-tail '() 0) => '())
;; 各种数据类型边界测试
(check (list-tail '(42 "text" #t 'symbol) 1)
  =>
  '("text" #t 'symbol)
) ;check
(check (list-tail '(#	 #
 #
) 0)
  =>
  '(#	 #
 #
)
) ;check
(check (list-tail '(#	 #
 #
) 2)
  =>
  '(#
)
) ;check
(check (list-tail '(#	 #
 #
) 3) => '())
;; 子列表包含嵌套结构测试
(check (list-tail '((a b) (c d) (e f)) 0)
  =>
  '((a b) (c d) (e f))
) ;check
(check (list-tail '((a b) (c d) (e f)) 1)
  =>
  '((c d) (e f))
) ;check
(check (list-tail '((a b) (c d) (e f)) 2)
  =>
  '((e f))
) ;check
(check (list-tail '((a b) (c d) (e f)) 3)
  =>
  '()
) ;check
;; 复杂数据结构测试
(check (list-tail '(() a b 3 c) 1)
  =>
  '(a b 3 c)
) ;check
(check (list-tail '(#(1 2) 'symbol "string" 3.14)
         1
       ) ;list-tail
  =>
  '('symbol "string" 3.14)
) ;check
(check (list-tail '(1 "hello" (nested list) #t 3.14)
         2
       ) ;list-tail
  =>
  '((nested list) #t 3.14)
) ;check
;; 长列表性能测试
(check (list-tail '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
         15
       ) ;list-tail
  =>
  '(16 17 18 19 20)
) ;check
(check (list-tail '(1 2 3 4 5 6 7 8 9 10) 10)
  =>
  '()
) ;check
(check (list-tail '(1 2 3 4 5 6 7 8 9 10) 0)
  =>
  '(1 2 3 4 5 6 7 8 9 10)
) ;check
(check (list-tail '(a b c d e f g h i j k l m n o p q r s t)
         0
       ) ;list-tail
  =>
  '(a b c d e f g h i j k l m n o p q r s t)
) ;check
;; 列表与点对结构测试
(check (list-tail '(a b c) 0)
  =>
  '(a b c)
) ;check
(check (list-tail '(a b c) 1) => '(b c))
(check (list-tail '(a b c) 2) => '(c))
(check (list-tail '(a b c) 3) => '())
;; 字符串列表测试
(check (list-tail '("hello" "world" "test") 1)
  =>
  '("world" "test")
) ;check
(check (list-tail '("first" "second" "third" "fourth")
         3
       ) ;list-tail
  =>
  '("fourth")
) ;check
;; 符号列表测试
(check (list-tail '(define lambda if cond else)
         2
       ) ;list-tail
  =>
  '(if cond else)
) ;check
(check (list-tail '(car cdr cons list) 4)
  =>
  '()
) ;check
;; 数值列表测试
(check (list-tail '(1 2 3 4 5) 1)
  =>
  '(2 3 4 5)
) ;check
(check (list-tail '(1 2 3 4 5) 3)
  =>
  '(4 5)
) ;check
(check (list-tail '(1 2 3 4 5) 5)
  =>
  '()
) ;check
(check (list-tail '(1.5 2.5 3.5 4.5) 2)
  =>
  '(3.5 4.5)
) ;check
;; Unicode字符串列表测试
(check (list-tail '("中文" "美国" "日本" "韩国")
         2
       ) ;list-tail
  =>
  '("日本" "韩国")
) ;check
(check (list-tail '("测试" "验证" "调试")
         0
       ) ;list-tail
  =>
  '("测试" "验证" "调试")
) ;check
;; 构造器函数结果测试
(check (list-tail (list 1 2 3 4 5) 2)
  =>
  '(3 4 5)
) ;check
(check (list-tail (append '(1 2) '(3 4 5)) 3)
  =>
  '(4 5)
) ;check
(check (list-tail (reverse '(5 4 3 2 1)) 2)
  =>
  '(3 4 5)
) ;check
;; 错误参数测试
(check-catch 'wrong-type-arg
  (list-tail 123 0)
) ;check-catch
(check-catch 'wrong-type-arg
  (list-tail "string" 1)
) ;check-catch
(check-catch 'wrong-type-arg
  (list-tail #t 0)
) ;check-catch
;; 索引越界测试
(check-catch 'out-of-range
  (list-tail '(a b c) -1)
) ;check-catch
(check-catch 'out-of-range
  (list-tail '(a b c) 4)
) ;check-catch
(check-catch 'out-of-range
  (list-tail '(a) 2)
) ;check-catch
(check-catch 'out-of-range
  (list-tail '() 1)
) ;check-catch
(check-catch 'out-of-range
  (list-tail '(single) 2)
) ;check-catch
;; 参数错误测试
(check-catch 'wrong-number-of-args
  (list-tail)
) ;check-catch
(check-catch 'wrong-number-of-args
  (list-tail '(a b c))
) ;check-catch
(check-catch 'wrong-number-of-args
  (list-tail '(a b c) 1 2)
) ;check-catch
(check-catch 'wrong-number-of-args
  (list-tail '(a b c) 1 2 3)
) ;check-catch
;; =======================================
;; [201_12] list-tail 边界测试和数据兼容性测试
;; 根据 201_12.md 要求补充至少8个边界值测试用例
;; =======================================
;; 边界测试集1：空列表和单元素边界
(check (list-tail '() 0) => '())
(check-catch 'out-of-range
  (list-tail '() 1)
) ;check-catch
(check (list-tail '(single) 0)
  =>
  '(single)
) ;check
(check (list-tail '(single) 1) => '())
(check-catch 'out-of-range
  (list-tail '(single) 2)
) ;check-catch
;; 边界测试集2：极限长度和性能边界
(check (list-tail (make-list 1000 'x) 999)
  =>
  '(x)
) ;check
(check (list-tail (make-list 1000 'x) 1000)
  =>
  '()
) ;check
(check (list-tail (make-list 100 'test) 0)
  =>
  (make-list 100 'test)
) ;check
(check (list-tail (append '(a b c) (make-list 997 'x))
         3
       ) ;list-tail
  =>
  (make-list 997 'x)
) ;check
;; 边界测试集3：各种数据类型兼容性边界
(check (list-tail '(42 3.14 1/2 1.0+2.0i) 1)
  =>
  '(3.14 1/2 1.0+2.0i)
) ;check
(check (list-tail '(#t #f #\a "hello" 'symbol)
         2
       ) ;list-tail
  =>
  '(#\a "hello" 'symbol)
) ;check
(check (list-tail '(#(1 2) #u(255) (a b) car)
         1
       ) ;list-tail
  =>
  '(#u(255) (a b) car)
) ;check
(check (list-tail '(() (()) ((()))) 1)
  =>
  '((()) ((())))
) ;check
;; 边界测试集4：Unicode和特殊字符边界
(check (list-tail '("中文" "测试" "🚀" "字符串")
         2
       ) ;list-tail
  =>
  '("🚀" "字符串")
) ;check
(check (list-tail '(#\中 #\文 #\🚀 #\测)
         1
       ) ;list-tail
  =>
  '(#\文 #\🚀 #\测)
) ;check
(check (list-tail '(#\newline #\tab #\space #\return)
         2
       ) ;list-tail
  =>
  '(#\space #\return)
) ;check
;; 边界测试集5：点对结构和非正规列表边界
(check (list-tail '(a b c) 0)
  =>
  '(a b c)
) ;check
(check (list-tail '(a b c) 2) => '(c))
(check (list-tail '(a b . c) 1)
  =>
  '(b . c)
) ;check
(check (list-tail '(a . b) 0)
  =>
  '(a . b)
) ;check
;; 边界测试集6：构造器函数结果边界
(check (list-tail (list 1 2 3 4 5) 3)
  =>
  '(4 5)
) ;check
(check (list-tail (append '(a b) '(c d) '(e f))
         4
       ) ;list-tail
  =>
  '(e f)
) ;check
(check (list-tail (reverse '(e d c b a)) 2)
  =>
  '(c d e)
) ;check
(check (list-tail (cons 'head
                    (cons 'mid (cons 'tail '()))
                  ) ;cons
         1
       ) ;list-tail
  =>
  '(mid tail)
) ;check
;; 边界测试集7：过程对象和特殊值边界
(check (list-tail (list car cdr cons list) 2)
  =>
  (list cons list)
) ;check
(check (list-tail '(#t #f #t #f #t) 3)
  =>
  '(#f #t)
) ;check
(check (list-tail '(define lambda if cond else)
         3
       ) ;list-tail
  =>
  '(cond else)
) ;check
;; 边界测试集8：内存结构共享验证边界
(let ((original '(shared structure test)))
  (let ((tail-result (list-tail original 1)))
    (check tail-result => '(structure test))
    (check (eq? tail-result (cdr original))
      =>
      #t
    ) ;check
  ) ;let
) ;let
;; 边界测试集9：负数索引和异常边界
(check-catch 'out-of-range
  (list-tail '(a b c) -1)
) ;check-catch
(check-catch 'out-of-range
  (list-tail '(a b c) -10)
) ;check-catch
(check-catch 'out-of-range
  (list-tail '() -1)
) ;check-catch
;; 边界测试集10：类型错误边界
(check-catch 'wrong-type-arg
  (list-tail "not-a-list" 0)
) ;check-catch
(check-catch 'wrong-type-arg
  (list-tail 123 1)
) ;check-catch
(check-catch 'wrong-type-arg
  (list-tail #t 0)
) ;check-catch
(check-catch 'wrong-type-arg
  (list-tail #(a b c) 1)
) ;check-catch
;; 补充边界测试：复杂嵌套和深度结构
(check (list-tail '((a (b (c))) d (e (f))) 1)
  =>
  '(d (e (f)))
) ;check
(check (list-tail '(1 2 (3 (4 (5))) 6 7) 2)
  =>
  '((3 (4 (5))) 6 7)
) ;check
(check (list-tail '((() (()) ((()))) ((((())))))
         2
       ) ;list-tail
  =>
  '()
) ;check
(check-report)