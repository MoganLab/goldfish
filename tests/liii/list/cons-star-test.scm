(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; cons*
;; 以线性更新方式构造列表，支持可变参数。
;;
;; 语法
;; ----
;; (cons* obj ...)
;;
;; 参数
;; ----
;; obj : any
;; 任意数量的参数，最少需要一个。
;;
;; 返回值
;; ----
;; list | pair
;; - 如果只有一个参数obj，返回该obj本身
;; - 如果有两个参数obj1 obj2，返回 (obj1 . obj2)
;; - 如果有三个或以上参数，返回一个列表，其中最后一个参数作为cdr
;;
;; 特性
;; ----
;; cons* 是一个灵活的列表构造函数，可以处理以下情况：
;; 1. 构造带点对的列表： (cons* 1 2 3) -> (1 2 . 3)
;; 2. 在列表前添加元素： (cons* 1 2 '(3 4)) -> (1 2 3 4)
;; 3. 处理嵌套结构： (cons* 1 '(2 3) 4) -> (1 (2 3) . 4)
;; 4. 处理空列表： (cons* 1 '() 2) -> (1 () . 2)
;;
;; 注意事项
;; ----
;; - cons* 单次使用时至少需要提供一个参数
;; - 当最后一个参数是列表时，结果是一个合法的列表
;; - 当最后一个参数不是列表时，结果是一个点对
;; - cons* 可以递归使用以构建复杂结构
;;
;; 示例
;; ----
;; (cons* 1) => 1
;; (cons* 1 2) => (1 . 2)
;; (cons* 1 2 3) => (1 2 . 3)
;; (cons* 1 2 3 4) => (1 2 3 . 4)
;; (cons* 1 '(2 3)) => (1 2 3)
;; (cons* 1 2 '(3 4)) => (1 2 3 4)
;; (cons* 1 2 3 '(4 5)) => (1 2 3 4 5)
;; (cons* 'a 'b 'c) => '(a b . c)
;; (cons* 'a () 'b) => '(a () . b)
;;
;; 错误处理
;; ----
;; wrong-number-of-args 如果没有提供任何参数
(check (cons* 1 2) => '(1 . 2))
(check (cons* 1 2 3) => '(1 2 . 3))
(check (cons* 'a 'b 'c 'd)
  =>
  '(a b c . d)
) ;check
(check (cons* '(1 2 3)) => '(1 2 3))
(check (cons* '(1 2) 3 4)
  =>
  '((1 2) 3 . 4)
) ;check
(check (cons* 1 2 '(3 4)) => '(1 2 3 4))
(check (cons* '(1) '(2) '(3))
  =>
  '((1) (2) 3)
) ;check
(check (cons* 1 '() 3) => '(1 () . 3))
(check (cons* 1 (cons* 2 3))
  =>
  '(1 2 . 3)
) ;check


(check (cons* 1) => 1)
(check (cons* 'a) => 'a)
(check (cons* '()) => '())
(check (cons* '(1 2 3)) => '(1 2 3))
(check (cons* '(a b) '(c d))
  =>
  '((a b) c d)
) ;check


(check (cons* 1 2 '()) => '(1 2))
(check (cons* '() '() '()) => '(() ()))
(check (cons* 1 2 3 4 '())
  =>
  '(1 2 3 4)
) ;check
(check (cons* 1 2 3 4 5)
  =>
  '(1 2 3 4 . 5)
) ;check


(check (cons* 1 2 (cons* 3 4 5))
  =>
  '(1 2 3 4 . 5)
) ;check
(check (cons* (cons* 1 2) 3 4)
  =>
  '((1 . 2) 3 . 4)
) ;check
(check (cons* 1 (list 2 3) (cons* 4 5))
  =>
  '(1 (2 3) 4 . 5)
) ;check


(check (cons* 'a 'b 'c) => '(a b . c))
(check (cons* 1 2 3 4 5 6)
  =>
  '(1 2 3 4 5 . 6)
) ;check


(check (cons* 1 'a 2 'b)
  =>
  '(1 a 2 . b)
) ;check
(check (cons* 'hello 42 'world)
  =>
  '(hello 42 . world)
) ;check


(check-catch 'wrong-number-of-args
  (cons*)
) ;check-catch


(check-report)
