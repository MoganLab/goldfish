(import (liii check)
        (scheme base)
)

(check-set-mode! 'report-failed)

#|
list-tail
返回列表从指定索引位置开始的子列表（尾部）。该函数是R7RS标准的基本列表操作函数之一。

语法
----
(list-tail list k)

参数
----
list : pair?
    非空列表或点结构。可以是普通列表、嵌套列表、点对结构或包含任意类型元素的列表。
    空列表 '() 作为参数时，仅当k=0时有效，其他情况抛出异常。

k : exact?
    非负的精确整数，表示要删除的元素个数（从列表头部开始）。
    必须满足 0 <= k <= (length list)。

返回值
------
list?
    从列表第k个位置开始直到列表末尾的子列表。当k=0时返回原列表本身，
    当k=length(list)时返回空列表'()。结果与原列表共享尾部内存结构。

说明
----
1. list-tail是从列表头部删除指定数量元素后得到的尾部子列表
2. 该函数采用结构共享策略，不会创建新的内存对象，直接返回原列表的尾部引用
3. 适用于所有类型的列表结构：普通列表、嵌套列表、点对结构、improper lists
4. 索引从0开始计数，与list-ref等其他列表函数保持一致
5. 功能与(cdr (cdr ... (cdr list)))等价，但通过循环实现更高效

边界条件
--------
- 空列表且k=0：返回空列表'()
- 单元素列表且k=0：返回原单元素列表
- 单元素列表且k=1：返回空列表'()
- k等于列表长度：返回空列表'()
- k超出列表长度：抛出out-of-range异常
- k为负数：抛出out-of-range异常
- 非列表参数：抛出wrong-type-arg异常
- 点对结构边界：正确处理非正规列表的截取

性能特征
--------
- 时间复杂度：O(k)，需要遍历前k个元素以定位截取位置
- 空间复杂度：O(1)，通过结构共享，无额外内存分配
- 内存分配：零分配策略，直接共享原始列表尾部内存结构
- 递归深度：非递归循环实现，避免栈溢出风险
- 对于长列表，性能与截取位置k成正比，与列表后续长度无关

数据类型兼容性
-------------
- 数值类型：整数、浮点数、复数、有理数等全数值类型支持
- 字符串类型：任意长度字符串，包括空字符串和Unicode字符串
- 字符类型：ASCII和Unicode字符统一支持
- 符号类型：普通符号、关键字符号、特殊符号
- 布尔类型：#t和#f标准支持
- 过程类型：内置过程和用户定义过程
- 复合类型：向量、字节向量、嵌套列表、点对结构
- 特殊对象：空列表、未指定值等特殊类型

错误处理
--------
out-of-range
    当索引k为负数或超出列表长度时抛出错误。
wrong-type-arg
    当list参数不是列表类型时抛出错误。
wrong-number-of-args
    当参数数量不等于2时抛出错误。

示例
----
(list-tail '(a b c d) 0) => '(a b c d)   ; 不删除元素，返回原列表
(list-tail '(a b c d) 2) => '(c d)       ; 删除前2个元素
(list-tail '(a b c d) 4) => '()          ; 删除所有元素，返回空列表
(list-tail '((a b) (c d) (e f)) 1) => '((c d) (e f))  ; 嵌套列表处理
|#

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
(check (list-tail '(42 "text" #t 'symbol) 1) => '("text" #t 'symbol))
(check (list-tail '(#	 #
 #
) 0) => '(#	 #
 #
))
(check (list-tail '(#	 #
 #
) 2) => '(#
))
(check (list-tail '(#	 #
 #
) 3) => '())

;; 子列表包含嵌套结构测试
(check (list-tail '((a b) (c d) (e f)) 0) => '((a b) (c d) (e f)))
(check (list-tail '((a b) (c d) (e f)) 1) => '((c d) (e f)))
(check (list-tail '((a b) (c d) (e f)) 2) => '((e f)))
(check (list-tail '((a b) (c d) (e f)) 3) => '())

;; 复杂数据结构测试
(check (list-tail '(() a b 3 c) 1) => '(a b 3 c))
(check (list-tail '(#(1 2) 'symbol "string" 3.14) 1) => '('symbol "string" 3.14))
(check (list-tail '(1 "hello" (nested list) #t 3.14) 2) => '((nested list) #t 3.14))

;; 长列表性能测试
(check (list-tail '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 15) => '(16 17 18 19 20))
(check (list-tail '(1 2 3 4 5 6 7 8 9 10) 10) => '())
(check (list-tail '(1 2 3 4 5 6 7 8 9 10) 0) => '(1 2 3 4 5 6 7 8 9 10))
(check (list-tail '(a b c d e f g h i j k l m n o p q r s t) 0) => '(a b c d e f g h i j k l m n o p q r s t))

;; 列表与点对结构测试
(check (list-tail '(a . (b . (c . ()))) 0) => '(a b c))
(check (list-tail '(a . (b . (c . ()))) 1) => '(b c))
(check (list-tail '(a . (b . (c . ()))) 2) => '(c))
(check (list-tail '(a . (b . (c . ()))) 3) => '())

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
(check (list-tail '("中文" "美国" "日本" "韩国") 2) => '("日本" "韩国"))
(check (list-tail '("测试" "验证" "调试") 0) => '("测试" "验证" "调试"))

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
(check (list-tail '() 0) => '())                    ; 边界：空列表k=0
(check-catch 'out-of-range (list-tail '() 1))       ; 边界：空列表k>0
(check (list-tail '(single) 0) => '(single))        ; 边界：单元素k=0
(check (list-tail '(single) 1) => '())              ; 边界：单元素k=1
(check-catch 'out-of-range (list-tail '(single) 2)) ; 边界：单元素k超界

;; 边界测试集2：极限长度和性能边界  
(check (list-tail (make-list 1000 'x) 999) => '(x))     ; 边界：极大长度临界点
(check (list-tail (make-list 1000 'x) 1000) => '())     ; 边界：极大长度边界
(check (list-tail (make-list 100 'test) 0) => (make-list 100 'test))  ; 边界：长列表k=0
(check (list-tail (append '(a b c) (make-list 997 'x)) 3) => (make-list 997 'x))  ; 边界：长列表性能

;; 边界测试集3：各种数据类型兼容性边界
(check (list-tail '(42 3.14 1/2 1+2i) 1) => '(3.14 1/2 1+2i))        ; 边界：数值类型
(check (list-tail '(#t #f #\a "hello" 'symbol) 2) => '(#\a "hello" 'symbol))  ; 边界：混合基础类型
(check (list-tail '(#(1 2) #u8(255) (a b) car) 1) => '(#u8(255) (a b) car))   ; 边界：复合对象类型
(check (list-tail '(() (()) ((()))) 1) => '((()) ((()))))              ; 边界：嵌套空列表结构

;; 边界测试集4：Unicode和特殊字符边界
(check (list-tail '("中文" "测试" "🚀" "字符串") 2) => '("🚀" "字符串"))     ; 边界：Unicode字符串
(check (list-tail '(#\中 #\文 #\🚀 #\测) 1) => '(#\文 #\🚀 #\测))       ; 边界：Unicode字符
(check (list-tail '(#\newline #\tab #\space #\return) 2) => '(#\space #\return))  ; 边界：特殊控制字符

;; 边界测试集5：点对结构和非正规列表边界
(check (list-tail '(a . (b . (c . ()))) 0) => '(a b c))      ; 边界：标准点对链表
(check (list-tail '(a . (b . (c . ()))) 2) => '(c))          ; 边界：点对链表截取
(check (list-tail '(a b . c) 1) => '(b . c))                 ; 边界：improper list
(check (list-tail '(a . b) 0) => '(a . b))                   ; 边界：纯点对结构

;; 边界测试集6：构造器函数结果边界
(check (list-tail (list 1 2 3 4 5) 3) => '(4 5))                           ; 边界：list构造器
(check (list-tail (append '(a b) '(c d) '(e f)) 4) => '(e f))              ; 边界：append结果
(check (list-tail (reverse '(e d c b a)) 2) => '(c d e))                   ; 边界：reverse结果
(check (list-tail (cons 'head (cons 'mid (cons 'tail '()))) 1) => '(mid tail))  ; 边界：cons链

;; 边界测试集7：过程对象和特殊值边界
(check (list-tail (list car cdr cons list) 2) => (list cons list))         ; 边界：过程对象列表
(check (list-tail '(#t #f #t #f #t) 3) => '(#f #t))                       ; 边界：布尔值序列
(check (list-tail '(define lambda if cond else) 3) => '(cond else))        ; 边界：关键字符号

;; 边界测试集8：内存结构共享验证边界
(let ((original '(shared structure test)))
  (let ((tail-result (list-tail original 1)))
    (check tail-result => '(structure test))                                ; 边界：结果正确性
    (check (eq? tail-result (cdr original)) => #t)))                        ; 边界：内存共享验证

;; 边界测试集9：负数索引和异常边界
(check-catch 'out-of-range (list-tail '(a b c) -1))                        ; 边界：负数索引
(check-catch 'out-of-range (list-tail '(a b c) -10))                       ; 边界：大负数索引
(check-catch 'out-of-range (list-tail '() -1))                             ; 边界：空列表负索引

;; 边界测试集10：类型错误边界
(check-catch 'wrong-type-arg (list-tail "not-a-list" 0))                   ; 边界：字符串参数
(check-catch 'wrong-type-arg (list-tail 123 1))                            ; 边界：数字参数  
(check-catch 'wrong-type-arg (list-tail #t 0))                             ; 边界：布尔参数
(check-catch 'wrong-type-arg (list-tail #(a b c) 1))                       ; 边界：向量参数

;; 补充边界测试：复杂嵌套和深度结构
(check (list-tail '((a (b (c))) d (e (f))) 1) => '(d (e (f))))             ; 边界：深度嵌套结构
(check (list-tail '(1 2 (3 (4 (5))) 6 7) 2) => '((3 (4 (5))) 6 7))       ; 边界：混合深度结构
(check (list-tail '((() (()) ((()))) ((((()))))) 2) => '())  ; 边界：极度嵌套空结构


(check-report)
