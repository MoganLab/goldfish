(import (liii check)
        (scheme base)
) ;import

(check-set-mode! 'report-failed)

#|
list?
判断给定的对象是否为列表类型。

语法
----
(list? obj)

参数
----
obj : any
任意类型的对象

返回值
------
boolean?
如果obj是列表类型则返回#t，否则返回#f

说明
----
1. 用于检查对象是否为列表类型
2. 能够正确识别空列表 '() 和非空列表
3. 能够处理嵌套列表和点对结构
4. 能够处理循环列表等特殊结构

特殊规则
---------
- 空列表 '() 被认为是列表
- 点对结构如果形成完整列表则也认为是列表
- 其他类型如数字、字符串、向量、布尔值等都返回#f
  

错误处理
---------
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; list? 基本测试：空列表和各种简单列表
(check-true (list? '()))                        ; 空列表
(check-true (list? '(a)))                       ; 单元素
(check-true (list? '(a b c)))                   ; 多元素普通列表
(check-true (list? '(1 2 3 4 5)))               ; 数字长列表

;; list? 嵌套和复杂结构测试
(check-true (list? '(a (b) c)))                 ; 嵌套列表  
(check-true (list? '((a) (b) (c))))             ; 多层嵌套
(check-true (list? '((a b) (c d))))             ; 深度嵌套
(check-true (list? '(1 (2 (3 (4))))))           ; 多级嵌套

;; list? 混合类型元素测试
(check-true (list? '(a 1 "string" #t)))         ; 混合类型
(check-true (list? '((list 1 2) (vector 3 4)))) ; 包含复杂对象

;; list? 点和边界情况
(check-true (list? '(1 . 2)))                   ; 点对结构
(check-true (list? '(a b . c)))                 ; 非完整列表边缘情况

;; list? 特殊结构测试
(check-true (list? (let ((x '(1 2 3)))          ; 循环列表
                    (set-cdr! (cddr x) x) x))
) ;check-true

;; list? 非列表类型测试 - 全面覆盖
(check-false (list? #t))                        ; 布尔值
(check-false (list? #f))                        ; 布尔值
(check-false (list? 123))                       ; 整数
(check-false (list? -456))                      ; 负整数
(check-false (list? 0))                         ; 零
(check-false (list? 3.14))                      ; 浮点数
(check-false (list? "Hello"))                   ; 字符串
(check-false (list? ""))                        ; 空字符串
(check-false (list? '#()))                       ; 空向量
(check-false (list? '#(1 2 3)))                  ; 向量
(check-false (list? '12345))                    ; 数字符号
(check-false (list? 'symbol))                   ; 符号
(check-false (list? #\a))                       ; 字符

;; list? 错误处理测试
(check-catch 'wrong-number-of-args (list?))
(check-catch 'wrong-number-of-args (list? #t #f))


(check-report)
