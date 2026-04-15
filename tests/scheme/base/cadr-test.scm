(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; cadr 基础测试 - 各种典型场景
(check (cadr '(a b)) => 'b)
(check (cadr '(a b c)) => 'b)
(check (cadr '(1 2 3 4 5)) => 2)
(check-catch 'wrong-type-arg (cadr '(a . b)))
(check (cadr '(a b . rest)) => 'b)
(check (cadr '((a . b) c)) => 'c)
;; cadr 边界测试
(check (cadr '(a b)) => 'b)
(check-catch 'wrong-type-arg (cadr '(only)))
(check (cadr '(pair single)) => 'single)
(check-catch 'wrong-type-arg (cadr '(a . b)))
;; 数据类型边界测试
(check (cadr '(42 string symbol #t)) => 'string)
(check (cadr '("hello" "world" "test")) => "world")
(check (cadr '(#t #f #t)) => #f)
(check (cadr '(list vector string)) => 'vector)
(check (cadr '((a b) (c d) (e f))) => '(c d))
;; 数值边界测试
(check (cadr '(100 200 300 400 500)) => 200)
(check (cadr '(1.1 2.2 3.3 4.4 5.5)) => 2.2)
(check (cadr '(1/2 2/3 3/4)) => 2/3)
(check (cadr '(1.0+2.0i 3.0+4.0i 5.0+6.0i)) => 3.0+4.0i)
;; 任意对象类型测试
(check (cadr '(#a "string" 42)) => "string")
(check (cadr '(if-cond then-block else-block))
  =>
  'then-block
) ;check
(check (cadr '((lambda (x) x) (lambda (y) y) (lambda (z) z)))
  =>
  '(lambda (y) y)
) ;check
;; 构造器创建的结构测试
(check (cadr (list 'a 'b 'c 'd)) => 'b)
(check (cadr (cons 'a (cons 'b (cons 'c '())))) => 'b)
(check (cadr (append '() '(a b c))) => 'b)
(check (cadr (reverse '(c b a))) => 'b)
;; Unicode和字符串边界测试
(check (cadr '("中文" "测试" "验证")) => "测试")
(check (cadr '(#\中 #\文 #\字)) => #\文)
(check-catch 'wrong-type-arg (cadr '()))
(check-catch 'wrong-type-arg (cadr 123))
(check-catch 'wrong-type-arg (cadr "string"))
(check-catch 'wrong-type-arg (cadr #t))
(check-catch 'wrong-type-arg (cadr #\a))
(check-catch 'wrong-type-arg (cadr #(a b)))
;; 单元素边界异常测试
(check-catch 'wrong-type-arg (cadr '(single)))
(check-catch 'wrong-type-arg (cadr '(all)))
;; 构造器与操作函数链式测试
(check (cadr (list 'car 'cdr 'cons 'append)) => 'cdr)
;; =======================================
;; [201_12] cadr 补充边界测试和文档完善
;; 根据 201_12.md 要求补充边界值和数据兼容性测试
;; =======================================
;; 边界测试集1：空结构边界
(check-catch 'wrong-type-arg (cadr (cons 'a 'b)))
(check-catch 'wrong-type-arg (cadr (cons 'a '())))
;; 边界测试集2：极限长度边界
(check (cadr (make-list 1000 'x)) => 'x)
(check (cadr (append '(a) (make-list 999 'x))) => 'x)
;; 边界测试集3：特殊对象类型边界
(check (cadr '(#t + #f)) => '+)
(check (cadr '(#t #(1 2 3) #f)) => #(1 2 3))
(check (cadr '(#t #u(255 128) #f)) => #u(255 128))
;; 边界测试集4：Unicode边界测试
(check (cadr '("特殊&符号" "正常字符串"))
  =>
  "正常字符串"
) ;check
;; 边界测试集5：复合结构异常边界
(check-catch 'wrong-type-arg (cadr (vector 'a 'b)))
(check-catch 'wrong-number-of-args (cadr))
(check-catch 'wrong-number-of-args (cadr '(a b) '(c d)))
(check-report)