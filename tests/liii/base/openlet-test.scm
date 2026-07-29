(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; openlet
;; 标记一个 let（环境）为 "open"，使内建函数会查询该 let 是否覆盖了某个方法。
;; 返回该 let 自身。
;;
;; 语法
;; ----
;; (openlet let)
;;
;; 参数
;; ----
;; let : let?
;; 要标记为 open 的 let。
;;
;; 返回值
;; ------
;; let?
;; 返回传入的 let（已被标记为 open）。
;;
;; 说明
;; ----
;; openlet 的唯一作用：让内建函数（length、object->string、display、
;; copy 等忽略参数类型的函数）遇到该对象时，查询对象内部同名方法并调用。
;; 这称为"方法分派"。
;;
;; coverlet 可撤销 openlet 的效果。
;; openlet 就地修改并返回原 let。
;;
;; 注意
;; ----
;; 1. 仅对"忽略参数类型"的内建函数生效；带类型检查的函数（如 abs 要求
;;    参数为 number）不会查询 openlet 的方法。
;; 2. openlet 不对用户自定义函数生效——自定义函数调用不会触发方法分派。
;; 3. 做面向对象 / data class 通常用 inlet + 顶层函数即可，无需 openlet。
;;    只有需要改写内建函数行为时才用 openlet。


;; openlet 返回传入的 let（eq? 为 #t）
(check (let ((e (inlet 'a 1))) (eq? (openlet e) e)) => #t)


;; openlet 标记后，openlet? 返回 #t
(check (let ((e (inlet 'a 1))) (openlet e) (openlet? e)) => #t)


;; 未经 openlet 的 inlet，openlet? 为 #f
(check (openlet? (inlet 'a 1)) => #f)


;; openlet 后内部绑定仍可访问
(check (let-ref (openlet (inlet 'a 42)) 'a) => 42)


;; 关键用法 1：length 方法分派
;; 纯 inlet 的 :length 方法不会被 length 调用，返回绑定数
(check (length (inlet 'length (lambda (x) 99) 'a 1)) => 2)

;; openlet 标记后，length 查询到 :length 方法并调用，返回 99
(check (length (openlet (inlet 'length (lambda (x) 99)))) => 99)


;; 关键用法 2：object->string 方法分派
;; 自定义对象的字符串表示（display 时也走此方法）
(check (object->string (openlet (inlet 'object->string (lambda args "#<myobj>"))))
  =>
  "#<myobj>"
) ;check


;; coverlet 可撤销 openlet 的方法分派效果
;; 撤销后 length 不再调用 :length 方法，返回绑定数
(check (length (coverlet (openlet (inlet 'length (lambda (x) 99))))) => 1)


(check-report)
