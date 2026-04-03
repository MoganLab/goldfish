(import (liii check)
        (scheme base)
) ;import

(check-set-mode! 'report-failed)

#|
list
通过给定元素构建一个新的列表。该函数是R7RS标准的基本列表构造函数之一。

语法
----
(list obj ...)

参数
----
obj ... : any
任意数量和类型的对象，包括符号、数字、字符串、布尔值、列表、向量、字节向量、字符、过程等。

返回值
------
list?
返回一个新的列表，包含所有参数obj按顺序排列。空参数返回空列表'()。

说明
----
1. list是构建新列表的首选构造函数，将任意数量和类型的参数组合成列表
2. 返回的是新创建的对象，与输入参数不会共享内存结构
3. 接受任意类型的参数，包括嵌套列表、复杂对象等特殊类型
4. 空参数调用返回空列表'()，单参数创建单元素列表
5. 与cons不同，list构造的是统一结构的列表

边界条件
--------
- 空参数：返回空列表'()
- 单参数：返回单元素列表
- 同类型参数：保持类型一致性
- 多类型混合：允许任意类型组合
- 嵌套结构：正确处理子列表嵌套
- 极大参数数量：支持大量参数构造
- 重复元素：保留每个实例

性能特征
--------
- 时间复杂度：O(n)，与参数数量成正比
- 空间复杂度：O(n)，新列表消耗内存与参数数量成正比
- 内存分配：创建新的序对对象，不共享输入参数
- 深层嵌套：支持深层结构不影响性能

数据类型兼容性
-------------
- 数值类型：整数、实数、复数、有理数
- 字符串：任意长度和内容字符串
- 字符：任意字符包括Unicode
- 符号：普通符号及关键字符号
- 布尔值：#t和#f支持
- 列表：空列表和任意嵌套列表
- 向量：任意维度和内容的向量
- 字节向量：任意字节序列
- 过程：内置过程和自定义过程
- 复合对象：对象组合的任意嵌套

限制说明
--------
- 参数传递是值传递，复杂对象会复制引用
- 极大参数列表可能导致内存不足
- 不支持单独设置元素值，如需精确控制需使用cons

示例
----
(list) => '()
(list 'a) => '(a)
(list 1 2 3) => '(1 2 3)
(list "hello" "world") => '("hello" "world")
(list 'a 42 #t 'b) => '(a 42 #t b)
|#

;; list 基本构造功能测试
(check (list) => '())                    ; 空参数构造
(check (list 'a) => '(a))               ; 单元素构造
(check (list 1 2 3) => '(1 2 3))        ; 多元素构造
(check (list "hello") => '("hello"))   ; 单元素字符串

;; 边界条件测试
(check (list) => '())                   ; 边界：空列表构造
(check (list 'single) => '(single))     ; 边界：单元素列表
(check (list 'a 'b 'c 'd) => '(a b c d))  ; 边界：四元素列表

;; 数据类型兼容性测试
(check (list 42 3.14 1/2 1+2i) => '(42 3.14 1/2 1+2i))  ; 数值类型
(check (list #t #f) => '(#t #f))        ; 布尔值
(check (list #\a #\b #\c) => '(#\a #\b #\c))   ; 字符
(check (list "hello" "world") => '("hello" "world"))  ; 字符串
(check (list 'symbol1 'symbol2 'keyword:) => '(symbol1 symbol2 keyword:)) ; 符号

;; 嵌套结构测试
(check (list '() '() '()) => '(() () ()))   ; 空列表嵌套
(check (list '(a b) '(c d)) => '((a b) (c d)))  ; 子列表嵌套
(check (list (list 1 2) (list 3 4)) => '((1 2) (3 4)))  ; 列表构造函数嵌套

;; 复杂对象测试
(check (list #(1 2 3) #u8(255 128)) => `(#(1 2 3) #u8(255 128)))  ; 向量/字节向量
(check (list #t #f) => `(#t #f))  ; 简单类型验证
(check (list 1 2 3) => `(1 2 3))  ; 数值类型验证

;; 混合类型测试
(check (list 'symbol 42 "text" #\c #t #(1 2)) => '(symbol 42 "text" #\c #t #(1 2)))  ; 全类型混合
(check (list 'a 1 "hello" '(sub list) #t 3.14) => '(a 1 "hello" (sub list) #t 3.14))  ; 复合混合

;; 极大参数数量测试
(check (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) => '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))  ; 边界：大量参数

;; 重复元素测试
(check (list 'a 'a 'a) => '(a a a))     ; 重复符号
(check (list 1 1 1 1) => '(1 1 1 1))     ; 重复数字
(check (list "test" "test") => '("test" "test"))  ; 重复字符串

;; Unicode和特殊字符测试
(check (list "中文" "测试" "字符串") => '("中文" "测试" "字符串"))  ; Unicode字符串
(check (list #\中 #\文) => '(#\中 #\文))   ; Unicode字符

;; 深层嵌套结构测试
(check (list (list (list 'a)) (list 'b)) => '(((a)) (b)))  ; 三层嵌套
(check (list '() (list '() (list 'a))) => '(() (() (a))))   ; 复杂嵌套

;; 性能验证：大列表构造
(check (length (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm 'n 'o 'p 'q 'r 's 't 'u 'v 'w 'x 'y 'z)) => 26)  ; 验证完整构造

;; 错误测试 - 参数类型验证
(check (list 123) => '(123))  ; 证实数字可以作为参数
(check (list "string") => '("string"))  ; 证实字符串可以作为参数
(check (list #t) => '(#t))   ; 证实布尔值可以作为参数

;; 构造后操作验证
(let ((constructed (list 1 2 3 4 5)))
  (check (length constructed) => 5)      ; 验证长度
  (check (list? constructed) => #t)      ; 验证类型
  (check (list-ref constructed 2) => 3)  ; 验证索引访问
) ;let

;; 独立对象验证 - 确认不与参数共享
(let ((a 'original)
      (b "test")
      (c #t))
  (let ((result (list a b c)))
    (check (equal? result '(original "test" #t)) => #t)  ; 值正确
    (check (not (eq? result a)) => #t)  ; 独立对象验证
    (check (not (eq? result b)) => #t)
  ) ;let
) ;let

;; 参数传递验证测试
(define (test-list-wrapper . args)
  (apply list args)
) ;define

(check (test-list-wrapper 1 2 3) => '(1 2 3))
(check (test-list-wrapper 'a 'b 'c 'd) => '(a b c d))
(check (test-list-wrapper) => '())



(check-report)
