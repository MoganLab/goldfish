(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

#|
pair?
判断一个对象是否为序对（pair）结构。

语法
----
(pair? obj)

参数
----
obj : any
任意类型的对象，包括原子、序对、列表、向量、字节向量、字符、字符串、符号、过程等。

返回值
-----
boolean?
如果 obj 是序对类型则返回 #t，否则返回 #f。

说明
----
判断一个对象是否为序对的基本谓词函数。序对是 Scheme 中最基础的数据结构，
由两个元素组成，形成一个双向的单元结构。序对可以是显式的点对形式，
如 (a . b)，也可以是非空列表，因为所有非空列表本质上都是由序对链组成的。

边界条件
--------
- 空列表 '() 返回 #f
- 单元素列表 '(a) 返回 #t（列表由一个序对构成）
- 显式点对 (a . b) 返回 #t
- 嵌套深度不影响结果：深度嵌套列表都返回 #t
- 任意类型的 car/cdr 值不影响返回值判断

性能特征
--------
- 时间复杂度：O(1) 恒定时间完成类型检查
- 空间复杂度：O(1) 不消耗额外栈空间
- 递归深度：0，这是一个基本谓词不会触发递归

数据类型兼容性
-------------
- 数值类型：整数、实数、复数都返回 #f
- 符号类型：普通符号和关键字符号都返回 #f  
- 字符串：所有字符串类型返回 #f
- 字符：所有字符类型返回 #f
- 布尔值：#t 和 #f 都返回 #f
- 过程：所有过程对象返回 #f
- 向量和字节向量：返回 #f
- 空列表：返回 #f
- 非空列表：返回 #t
- 点对结构：返回 #t

注意
----
- 空列表 '() 不是序对
- 字符串、数字、布尔值等原子类型都不是序对
- 所有非空列表都被认为是序对，因为列表本质上是由序对链构成的
- 这一过程对存储的 car/cdr 值内容不做任何验证

|#

;; 测试 pair? 对各种序对结构的判断
(check-true (pair? '(a . b)))             ; 显式点对形式的序对
(check-true (pair? '(a b c)))             ; 列表内部由序对构成
(check-true (pair? (cons 1 2)))           ; 使用 cons 创建的序对
(check-true (pair? (cons 'a (cons 'b 'c))))  ; 嵌套序对结构

(check-false (pair? 'a))
(check-false (pair? 123))
(check-false (pair? "string"))
(check-false (pair? #t))
(check-false (pair? #f))

;; pair? 边界条件测试补充
;; 基本边界值验证
(check-false (pair? '()))                                ; 空列表边界
(check-true (pair? '(single)))                           ; 单元素列表边界
(check-true (pair? (cons 1 '())))                        ; 单元素cons构建
(check-true (pair? '(())))                               ; 空列表作为唯一元素

;; 嵌套深度边界测试
(check-true (pair? '((((a))))) )                         ; 深度嵌套列表
(check-true (pair? (cons 'a (cons 'b (cons 'c '())))))   ; 深层cons链
(check-true (pair? '(a b (c d (e)))))                    ; 中度嵌套绑定

;; 数据类型兼容性边界测试
(check-false (pair? 42))                                ; 整数类型
(check-false (pair? 3.14))                              ; 实数类型
(check-false (pair? 1+2i))                              ; 复数类型
(check-false (pair? #t))                                ; 布尔真
(check-false (pair? #f))                                ; 布尔假  
(check-false (pair? "hello"))                           ; 字符串
(check-false (pair? #\a))                               ; 字符
(check-false (pair? 'symbol))                           ; 符号
(check-false (pair? 'quote))                            ; 特殊符号
(check-false (pair? +))                                 ; 过程对象
(check-false (pair? length))                            ; 过程对象

;; 复杂对象边界测试
(check-false (pair? #(1 2 3)))                          ; 向量对象
(check-false (pair? #u8(1 2 3)))                        ; 字节向量
(check-false (pair? (lambda (x) x)))                    ; lambda过程
(check-false (pair? #<eof>))                            ; 特殊对象

;; 极端边界测试
(check-true (pair? (cons '() '())))                      ; 空列表组成的序对
(check-true (pair? (cons #t #f)))                        ; 布尔值组成序对
(check-true (pair? (cons 42 "string")))                  ; 混合类型序对
(check-true (pair? (cons (cons 1 2) (cons 3 4))))        ; 嵌套序对组合

;; 构造器多样化测试
(check-true (pair? (list 1 2)))                          ; list构造器
(check-true (pair? (append '(1) '(2))))                  ; append结果
(check-true (pair? (cons 'a (list 'b 'c))))               ; 混合构造器

;; 结构性边界验证
(check-false (pair? 'a))                                 ; 原子符号
(check-false (pair? 1000000))                            ; 极大整数边界
(check-false (pair? "中文测试"))                            ; 多字节字符串
(check-false (pair? #\newline))                          ; 特殊字符

;; Improper list 边界验证
(check-true (pair? '(a . b)))                            ; 基础点对形式
(check-true (pair? '(a b . c)))                          ; 扩展点对形式
(check-true (pair? '(a b c . d)))                        ; 多点结构


(check-report)
