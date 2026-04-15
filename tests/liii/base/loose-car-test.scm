(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; loose-car
;; 宽松地获取列表的第一个元素（car）。
;; 与标准 car 不同，当传入空列表时返回空列表而不是报错。
;;
;; 语法
;; ----
;; (loose-car pair-or-empty)
;;
;; 参数
;; ----
;; pair-or-empty : pair? 或 null?
;; 一个配对（非空列表）或空列表。
;;
;; 返回值
;; ------
;; 如果参数是非空列表，返回其第一个元素；
;; 如果参数是空列表，返回空列表。


;; 测试非空列表
(check (loose-car '(1 2 3)) => 1)
(check (loose-car '(a b c)) => 'a)
(check (loose-car '("hello" "world"))
  =>
  "hello"
) ;check


;; 测试空列表（这是 loose-car 的主要用途）
(check (loose-car '()) => '())


;; 测试嵌套列表
(check (loose-car '((1 2) 3 4))
  =>
  '(1 2)
) ;check


;; 测试单元素列表
(check (loose-car '(only)) => 'only)


(check-report)
