(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)

;;
;; list?
;; 判断给定的对象是否为列表类型。
;;
;; 语法
;; ----
;; (list? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意类型的对象
;;
;; 返回值
;; ------
;; boolean?
;; 如果obj是列表类型则返回#t，否则返回#f
;;
;; 说明
;; ----
;; 1. 用于检查对象是否为列表类型
;; 2. 能够正确识别空列表 '() 和非空列表
;; 3. 能够处理嵌套列表和点对结构
;; 4. 能够处理循环列表等特殊结构
;;
;; 特殊规则
;; ---------
;; - 空列表 '() 被认为是列表
;; - 点对结构如果形成完整列表则也认为是列表
;; - 其他类型如数字、字符串、向量、布尔值等都返回#f
;;
;;
;; 错误处理
;; ---------
;; wrong-number-of-args
;; 当参数数量不为1时抛出错误。

;; list? 基本测试：空列表和各种简单列表
(check-true (list? '()))
(check-true (list? '(a)))
(check-true (list? '(a b c)))
(check-true (list? '(1 2 3 4 5)))
;; list? 嵌套和复杂结构测试
(check-true (list? '(a (b) c)))
(check-true (list? '((a) (b) (c))))
(check-true (list? '((a b) (c d))))
(check-true (list? '(1 (2 (3 (4))))))
;; list? 混合类型元素测试
(check-true (list? '(a 1 "string" #t)))
(check-true (list? '((list 1 2) (vector 3 4)))
) ;check-true
;; list? 点和边界情况
(check-true (list? '(1 . 2)))
(check-true (list? '(a b . c)))
;; list? 特殊结构测试
(check-true (list? (let ((x '(1 2 3)))
                     (set-cdr! (cddr x) x)
                     x
                   ) ;let
            ) ;list?
) ;check-true
;; list? 非列表类型测试 - 全面覆盖
(check-false (list? #t))
(check-false (list? #f))
(check-false (list? 123))
(check-false (list? -456))
(check-false (list? 0))
(check-false (list? 3.14))
(check-false (list? "Hello"))
(check-false (list? ""))
(check-false (list? '#()))
(check-false (list? '#(1 2 3)))
(check-false (list? '12345))
(check-false (list? 'symbol))
(check-false (list? #\a))
;; list? 错误处理测试
(check-catch 'wrong-number-of-args
  (list?)
) ;check-catch
(check-catch 'wrong-number-of-args
  (list? #t #f)
) ;check-catch
(check-report)