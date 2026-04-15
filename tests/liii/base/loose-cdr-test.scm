(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; loose-cdr
;; 宽松地获取列表的剩余部分（cdr）。
;; 与标准 cdr 不同，当传入空列表时返回空列表而不是报错。
;;
;; 语法
;; ----
;; (loose-cdr pair-or-empty)
;;
;; 参数
;; ----
;; pair-or-empty : pair? 或 null?
;; 一个配对（非空列表）或空列表。
;;
;; 返回值
;; ------
;; 如果参数是非空列表，返回除第一个元素外的剩余部分；
;; 如果参数是空列表，返回空列表。


;; 测试非空列表
(check (loose-cdr '(1 2 3)) => '(2 3))
(check (loose-cdr '(a b c)) => '(b c))
(check (loose-cdr '("hello" "world"))
  =>
  '("world")
) ;check


;; 测试空列表（这是 loose-cdr 的主要用途）
(check (loose-cdr '()) => '())


;; 测试嵌套列表
(check (loose-cdr '((1 2) 3 4))
  =>
  '(3 4)
) ;check


;; 测试单元素列表
(check (loose-cdr '(only)) => '())


;; 测试双元素列表
(check (loose-cdr '(first second))
  =>
  '(second)
) ;check


(check-report)
