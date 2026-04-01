(import (liii check)
        (scheme base)
) ;import

(check-set-mode! 'report-failed)

#|
make-list
创建一个包含指定数量指定元素的列表。

语法
----
(make-list n fill)
(make-list n)

参数
----
n : exact?
    精确的非负整数，表示要创建的列表长度。
    必须满足 0 <= n < (expt 2 32) 或实现定义的最大允许列表长度。

fill : any (可选)
    填充到列表中的元素值。如果不指定，默认为 #f。
    可以是任何类型的 Scheme 对象。

返回值
------
list?
    一个长度为 n 的列表，所有元素都为 fill 值。
    当 n 为 0 时返回空列表 '()。

说明
----
1. 这是一个特殊形式的列表构造器，用于快速创建包含重复元素的列表
2. 可为任何非负整数长度创建列表，包括空列表
3. 填充元素可以是任意类型，包括复杂对象
4. 使用 make-list 构造的列表是全新的单个对象，不会与其他对象共享结构
5. 主要用于初始化数据结构、生成测试数据等场景

边界条件
--------
- n = 0 返回空列表 '()
- n = 1 返回单元素列表 '(fill)
- 支持极大 n 值（受实现最大允许列表长度限制）
- fill 值可以是各种数据类型和复杂对象
- 允许列表内容为嵌套结构或函数对象

性能特征
--------
- 时间复杂度：O(n)，线性时间构建列表
- 空间复杂度：O(n)，需要为n个新序对分配内存
- 递归深度：无递归，使用循环构造
- 内存分配：创建n个新的序对对象

数据类型兼容性
-------------
- fill值支持：数值、布尔、字符、字符串、符号、过程、列表、向量、字节向量等所有类型
- n值约束：整数类型，建议为正整数和0
- 返回值：统一的列表类型

限制说明
--------
- fill参数是值传递，复杂对象会复制引用，不会创建新对象
- 极大n值可能导致内存不足
- 不适合动态增长的列表构建

示例
----
(make-list 3 'x) => '(x x x)
(make-list 0 'x) => '()
(make-list 2 42) => '(42 42)
(make-list 4 #\a) => '(#\a #\a #\a #\a)
|#

;; make-list 基本功能测试
(check (make-list 0) => '())                                   ; 空列表边界
(check (make-list 0 'x) => '())                               ; 指定填充值的空列表
(check (make-list 1) => '(#f))                                ; 默认填充值单元素
(check (make-list 1 'singleton) => '(singleton))             ; 自定义填充值单元素
(check (make-list 3) => '(#f #f #f))                          ; 默认填充值多元素
(check (make-list 3 'repeat) => '(repeat repeat repeat))      ; 自定义填充值多元素

;; make-list 边界值测试
(check (make-list 10 'a) => '(a a a a a a a a a a))           ; 到长度边界
(check (make-list 100 'X) => (make-list 100 'X))             ; 大长度验证一致性

;; make-list 数据类型兼容性测试
(check (make-list 4 42) => '(42 42 42 42))                    ; 整数类型
(check (make-list 3 3.14) => '(3.14 3.14 3.14))               ; 实数类型
(check (make-list 2 #t) => '(#t #t))                          ; 布尔值
(check (make-list 2 #\a) => '(#\a #\a))                      ; 字符类型
(check (make-list 2 "hello") => '("hello" "hello"))           ; 字符串类型
(check (make-list 3 'symbol) => '(symbol symbol symbol))      ; 符号类型
(check (make-list 2 #(1 2 3)) => `(#(1 2 3) #(1 2 3)))      ; 向量对象
(check (make-list 2 #u8(255 128)) => `(#u8(255 128) #u8(255 128))) ; 字节向量

;; make-list 复杂数据类型测试
(check (make-list 3 '()) => '(() () ()))                      ; 空列表作为填充值
(check (make-list 2 '(a b c)) => '((a b c) (a b c)))          ; 列表作为填充值
(check (make-list 2 (list 1 2 3)) => '((1 2 3) (1 2 3)))      ; 列表作为填充值

;; make-list 极端边界测试
(check (make-list 2 (cons 'a 'b)) => '((a . b) (a . b)))      ; 点对结构作为填充值
(check (make-list 1 (make-list 3 'x)) => '((x x x)))          ; 嵌套make-list调用
(check (make-list 3 (make-list 0)) => '(() () ()))            ; 空列表嵌套

;; make-list 函数和过程对象的基本验证
(check (length (make-list 3 car)) => 3)                      ; 验证长度
(check (list? (make-list 2 car)) => #t)                      ; 验证类型
(check (procedure? (car (make-list 2 car))) => #t)           ; 验证过程对象正确性

;; make-list 动态边界验证
(let ((n 5))
  (let ((result (make-list n 'test)))
    (check (length result) => n)
    (check (list? result) => #t)
    (check (car result) => 'test) ; 简单的元素验证
  ) ;let
) ;let

;; make-list 内存结构验证  
(let ((lst1 (make-list 3 'same))
      (lst2 (make-list 3 'same)))
  (check (equal? lst1 lst2) => #t)                          ; 内容相同
  (check (eq? lst1 lst2) => #f)                             ; 不是同一对象
) ;let

;; 错误参数类型测试
(check-catch 'wrong-number-of-args (make-list))               ; 参数不足
(check-catch 'wrong-number-of-args (make-list 3 'x 'extra))   ; 参数过多
(check-catch 'out-of-range (make-list -1 'x))                ; 负数长度错误


(check-report)
