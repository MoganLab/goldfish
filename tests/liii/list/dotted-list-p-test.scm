(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; dotted-list?
;; 判断一个对象是否为dotted list。
;;
;; 语法
;; ----
;; (dotted-list? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意对象
;;
;; 返回值
;; ----
;; boolean?
;; 如果obj是dotted list返回#t，否则返回#f。
;;
;; 说明
;; ----
;; dotted list是指不符合proper list规范但也不是循环列表的列表结构，主要特征包括：
;; 1. 以非空列表结尾的cons结构，如 (a . b) 或 (a b . c)
;; 2. 最终cdr部分不是空列表的列表
;; 3. 单个非pair/非null对象也被视为dotted list
;; 4. 空列表和proper list不是dotted list
;;
;; 使用场景
;; --------
;; 该函数用于区分proper list与其他非列表结构，特别是在处理列表输入验证时非常有用。
;;
;; 注意
;; ----
;; - 循环列表不被视为dotted list
;; - 该函数是proper-list?的补集（对于非循环列表而言）
;; - 可以用于检测输入是否应该被当作列表处理
;;
;; 错误处理
;; --------
;; 无特殊错误处理，任何类型的对象都能接受。


(check-true (dotted-list? 1))
(check-true (dotted-list? '(1 . 2)))
(check-true (dotted-list? '(1 2 . 3)))


(check-false (dotted-list? (circular-list 1 2 3))
) ;check-false
(check-false (dotted-list? '()))
(check-false (dotted-list? '(a)))
(check-false (dotted-list? '(a b)))
(check-false (dotted-list? '(a b (c d)))
) ;check-false


(check-true (dotted-list? 'a))
(check-true (dotted-list? 'symbol))
(check-true (dotted-list? "string"))
(check-true (dotted-list? 42))
(check-true (dotted-list? '(a . b)))
(check-true (dotted-list? '(a b . c)))
(check-true (dotted-list? '(a b c . d)))
(check-true (dotted-list? '(a b c d . e))
) ;check-true
(check-true (dotted-list? '(() . a)))
(check-true (dotted-list? '(a () . b)))
(check-true (dotted-list? '(a b () . c))
) ;check-true


(check-true (dotted-list? '((a . b) c . d))
) ;check-true
(check-true (dotted-list? '(a (b . c) . d))
) ;check-true
(check-true (dotted-list? '(a b . c)))


(check-false (dotted-list? '(())))
(check-false (dotted-list? '(a ())))
(check-false (dotted-list? '(() a b)))
(check-true (dotted-list? '(a () . b)))


(check-true (dotted-list? '(a b c . d)))
(check-false (dotted-list? '(a b (c . d)))
) ;check-false


(check-false (dotted-list? '()))
(check-false (dotted-list? '(a)))
(check-false (dotted-list? '(a b)))
(check-false (dotted-list? '(a b c)))
(check-true (dotted-list? '(a . b)))
(check-true (dotted-list? '(a b . c)))
(check-true (dotted-list? 'a))


(let ((lst (list 1 2 3)))
  (set-cdr! (last-pair lst) lst)
  (check-false (dotted-list? lst))
) ;let


(check-true (dotted-list? '(a (b . c) . d))
) ;check-true
(check-false (dotted-list? '((a b) (c d)))
) ;check-false
(check-true (dotted-list? '((a b) . c)))


(check-report)
