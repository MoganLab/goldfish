(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)

;; list-tail
;; 返回列表从指定索引位置开始的子列表（尾部）。该函数是 R7RS 标准的基本列表操作函数之一。
;;
;; 语法
;; ----
;; (list-tail list k)
;;
;; 参数
;; ----
;; list : pair?
;; 非空列表或点结构。空列表 '() 作为参数时，仅当 k=0 时有效。
;;
;; k : exact?
;; 非负的精确整数，表示要删除的元素个数（从列表头部开始）。
;;
;; 返回值
;; ------
;; list?
;; 从列表第 k 个位置开始直到列表末尾的子列表。当 k=0 时返回原列表本身，
;; 当 k=length(list) 时返回空列表 '()。
;;
;; 说明
;; ----
;; 1. list-tail 是从列表头部删除指定数量元素后得到的尾部子列表
;; 2. 适用于所有类型的列表结构：普通列表、嵌套列表、点对结构
;; 3. 索引从 0 开始计数，与 list-ref 等其他列表函数保持一致
;;
;; 错误处理
;; --------
;; out-of-range
;; 当索引 k 为负数或超出列表长度时抛出错误。
;; wrong-type-arg
;; 当 list 参数不是列表类型时抛出错误。
;; wrong-number-of-args
;; 当参数数量不等于 2 时抛出错误。

;; list-tail 基本功能测试
(check (list-tail '(a b c d) 0) => '(a b c d))
(check (list-tail '(a b c d) 1) => '(b c d))
(check (list-tail '(a b c d) 2) => '(c d))
(check (list-tail '(a b c d) 3) => '(d))
(check (list-tail '(a b c d) 4) => '())
;; 边界值测试
(check (list-tail '(single) 0) => '(single))
(check (list-tail '(single) 1) => '())
(check (list-tail '() 0) => '())
;; 各种数据类型边界测试
(check (list-tail '(42 "text" #t (#_quote symbol)) 1)
  =>
  '("text" #t (#_quote symbol))
) ;check
(check (list-tail '(#t #t #t) 0) => '(#t #t #t))
(check (list-tail '(#t #t #t) 2) => '(#t))
(check (list-tail '(#t #t #t) 3) => '())

;; 子列表包含嵌套结构测试
(check (list-tail '((a b) (c d) (e f)) 0) => '((a b) (c d) (e f)))
(check (list-tail '((a b) (c d) (e f)) 1) => '((c d) (e f)))
(check (list-tail '((a b) (c d) (e f)) 2) => '((e f)))
(check (list-tail '((a b) (c d) (e f)) 3) => '())
;; 复杂数据结构测试
(check (list-tail '(() a b 3 c) 1) => '(a b 3 c))
(check (list-tail '(#(1 2) (#_quote symbol) "string" 3.14) 1)
  =>
  '((#_quote symbol) "string" 3.14)
) ;check
(check (list-tail '(1 "hello" (nested list) #t 3.14) 2)
  =>
  '((nested list) #t 3.14)
) ;check
;; 长列表性能测试
(check (list-tail '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 15)
  =>
  '(16 17 18 19 20)
) ;check
(check (list-tail '(1 2 3 4 5 6 7 8 9 10) 10) => '())
(check (list-tail '(1 2 3 4 5 6 7 8 9 10) 0) => '(1 2 3 4 5 6 7 8 9 10))
(check (list-tail '(a b c d e f g h i j k l m n o p q r s t) 0)
  =>
  '(a b c d e f g h i j k l m n o p q r s t)
) ;check
;; 列表与点对结构测试
(check (list-tail '(a b c) 0) => '(a b c))
(check (list-tail '(a b c) 1) => '(b c))
(check (list-tail '(a b c) 2) => '(c))
(check (list-tail '(a b c) 3) => '())
;; 字符串列表测试
(check (list-tail '("hello" "world" "test") 1) => '("world" "test"))
(check (list-tail '("first" "second" "third" "fourth") 3) => '("fourth"))
;; 符号列表测试
(check (list-tail '(define lambda if cond else) 2) => '(if cond else))
(check (list-tail '(car cdr cons list) 4) => '())
;; 数值列表测试
(check (list-tail '(1 2 3 4 5) 1) => '(2 3 4 5))
(check (list-tail '(1 2 3 4 5) 3) => '(4 5))
(check (list-tail '(1 2 3 4 5) 5) => '())
(check (list-tail '(1.5 2.5 3.5 4.5) 2) => '(3.5 4.5))
;; Unicode字符串列表测试
(check (list-tail '("中文" "美国" "日本" "韩国") 2)
  =>
  '("日本" "韩国")
) ;check
(check (list-tail '("测试" "验证" "调试") 0)
  =>
  '("测试" "验证" "调试")
) ;check
;; 构造器函数结果测试
(check (list-tail (list 1 2 3 4 5) 2) => '(3 4 5))
(check (list-tail (append '(1 2) '(3 4 5)) 3) => '(4 5))
(check (list-tail (reverse '(5 4 3 2 1)) 2) => '(3 4 5))
;; 错误参数测试
(check-catch 'wrong-type-arg (list-tail 123 0))
(check-catch 'wrong-type-arg (list-tail "string" 1))
(check-catch 'wrong-type-arg (list-tail #t 0))
;; 索引越界测试
(check-catch 'out-of-range (list-tail '(a b c) -1))
(check-catch 'out-of-range (list-tail '(a b c) 4))
(check-catch 'out-of-range (list-tail '(a) 2))
(check-catch 'out-of-range (list-tail '() 1))
(check-catch 'out-of-range (list-tail '(single) 2))
;; 参数错误测试
(check-catch 'wrong-number-of-args (list-tail))
(check-catch 'wrong-number-of-args (list-tail '(a b c)))
(check-catch 'wrong-number-of-args (list-tail '(a b c) 1 2))
(check-catch 'wrong-number-of-args (list-tail '(a b c) 1 2 3))
;; =======================================
;; [201_12] list-tail 边界测试和数据兼容性测试
;; 根据 201_12.md 要求补充至少8个边界值测试用例
;; =======================================
;; 边界测试集1：空列表和单元素边界
(check (list-tail '() 0) => '())
(check-catch 'out-of-range (list-tail '() 1))
(check (list-tail '(single) 0) => '(single))
(check (list-tail '(single) 1) => '())
(check-catch 'out-of-range (list-tail '(single) 2))
;; 边界测试集2：极限长度和性能边界
(check (list-tail (make-list 1000 'x) 999) => '(x))
(check (list-tail (make-list 1000 'x) 1000) => '())
(check (list-tail (make-list 100 'test) 0) => (make-list 100 'test))
(check (list-tail (append '(a b c) (make-list 997 'x)) 3) => (make-list 997 'x))
;; 边界测试集3：各种数据类型兼容性边界
(check (list-tail '(42 3.14 1/2 1.0+2.0i) 1) => '(3.14 1/2 1.0+2.0i))
(check (list-tail '(#t #f #\a "hello" (#_quote symbol)) 2)
  =>
  '(#\a "hello" (#_quote symbol))
) ;check
(check (list-tail '(#(1 2) #(255) (a b) car) 1) => '(#(255) (a b) car))
(check (list-tail '(() (()) ((()))) 1) => '((()) ((()))))
;; 边界测试集4：Unicode和特殊字符边界
(check (list-tail '("中文" "测试" "🚀" "字符串") 2)
  =>
  '("🚀" "字符串")
) ;check
(check (list-tail '(#\中 #\文 #\🚀 #\测) 1) => '(#\文 #\🚀 #\测))
(check (list-tail '(#\newline #\tab #\space #\return) 2) => '(#\space #\return))
;; 边界测试集5：点对结构和非正规列表边界
(check (list-tail '(a b c) 0) => '(a b c))
(check (list-tail '(a b c) 2) => '(c))
(check (list-tail '(a b . c) 1) => '(b . c))
(check (list-tail '(a . b) 0) => '(a . b))
;; 边界测试集6：构造器函数结果边界
(check (list-tail (list 1 2 3 4 5) 3) => '(4 5))
(check (list-tail (append '(a b) '(c d) '(e f)) 4) => '(e f))
(check (list-tail (reverse '(e d c b a)) 2) => '(c d e))
(check (list-tail (cons 'head (cons 'mid (cons 'tail '()))) 1) => '(mid tail))
;; 边界测试集7：过程对象和特殊值边界
(check (list-tail (list car cdr cons list) 2) => (list cons list))
(check (list-tail '(#t #f #t #f #t) 3) => '(#f #t))
(check (list-tail '(define lambda if cond else) 3) => '(cond else))
;; 边界测试集8：内存结构共享验证边界
(let ((original '(shared structure test)))
  (let ((tail-result (list-tail original 1)))
    (check tail-result => '(structure test))
    (check (eq? tail-result (cdr original)) => #t)
  ) ;let
) ;let
;; 边界测试集9：负数索引和异常边界
(check-catch 'out-of-range (list-tail '(a b c) -1))
(check-catch 'out-of-range (list-tail '(a b c) -10))
(check-catch 'out-of-range (list-tail '() -1))
;; 边界测试集10：类型错误边界
(check-catch 'wrong-type-arg (list-tail "not-a-list" 0))
(check-catch 'wrong-type-arg (list-tail 123 1))
(check-catch 'wrong-type-arg (list-tail #t 0))
(check-catch 'wrong-type-arg (list-tail #(a b c) 1))
;; 补充边界测试：复杂嵌套和深度结构
(check (list-tail '((a (b (c))) d (e (f))) 1) => '(d (e (f))))
(check (list-tail '(1 2 (3 (4 (5))) 6 7) 2) => '((3 (4 (5))) 6 7))
(check (list-tail '((() (()) ((()))) ((((()))))) 2) => '())
(check-report)
