;; list?
;; 判断给定的对象是否为有限完全列表（proper list）。
;;
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
;; ----
;; boolean?
;; 如果obj是有限完全列表则返回#t，否则返回#f
;;
;; 说明
;; ----
;; 1. 用于检查对象是否为完全列表类型
;; 2. 能够正确识别空列表 '() 和非空完全列表
;; 3. 能够处理嵌套列表，元素的类型不影响判定
;; 4. 点对结尾（improper list）不是完全列表，返回#f
;;
;; 说明
;; ----
;; - 空列表 '() 被认为是列表
;; - 完全列表是由若干 pair 组成、且最后一个 cdr 为 '() 的链
;; - 点对结尾的结构（最后一个 cdr 不是 '()）返回#f
;; - 循环列表不是有限完全列表，返回#f
;; - 其他类型如数字、字符串、向量、布尔值等都返回#f
;;
;; 错误处理
;; ----
;; wrong-number-of-args
;; 当参数数量不为1时抛出错误。

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; list? 基本测试：空列表和各种简单列表
(check-true (list? '()))
(check-true (list? '(a)))
(check-true (list? '(a b c)))
(check-true (list? '(1 2 3 4 5)))
;; list? 嵌套和复杂结构测试
(check-true (list? '(a (b) c)))
(check-true (list? '((a) (b) (c))))
(check-true (list? '((a b) (c d))))
(check-true
  (list?
    '(1 (2 (3 (4))))
  ) ;list?
) ;check-true
;; list? 混合类型元素测试
(check-true (list? '(a 1 "string" #t)))
(check-true (list? '((list 1 2) (vector 3 4))))
;; list? 元素本身是点对不影响外层判定
(check-true (list? '((1 . 2))))
(check-true (list? '((1 . 2) (3 . 4))))
(check-true (list? (list '(1 . 2))))
;; list? 点对结尾（improper list）返回 #f
(check-false (list? '(1 . 2)))
(check-false (list? '(a b . c)))
(check-false (list? '(a b . c)))
(check-false (list? '((1 . 2) . 3)))
(check-false (list? '(1 2 . 3)))
(check-false (list? (cons 1 2)))
;; list? 特殊结构测试
(check-false
  (list?
    (let ((x '(1 2 3)))
      (set-cdr! (cddr x) x)
      x
    ) ;let
  ) ;list?
) ;check-false
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
(check-catch 'wrong-number-of-args (list?))
(check-catch 'wrong-number-of-args (list? #t #f))

(check-report)
