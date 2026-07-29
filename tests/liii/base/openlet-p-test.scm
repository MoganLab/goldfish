(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; openlet?
;; 判断 let 是否被 openlet 标记为 "open"。
;;
;; 语法
;; ----
;; (openlet? obj)
;;
;; 参数
;; ----
;; obj : any
;; 要判断的对象。
;;
;; 返回值
;; ------
;; boolean?
;; 如果 obj 是一个被 openlet 标记过的 let，返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; openlet? 判断一个 let 是否处于 open 状态。
;; open 状态下，内建函数作用于该 let 时会查询其是否覆盖了方法。
;; 新建的 inlet 默认不是 open。


;; 新建的 inlet 不是 openlet
(check (openlet? (inlet 'a 1)) => #f)


;; openlet 标记后的 let 是 openlet
(check (openlet? (openlet (inlet 'a 1))) => #t)


;; coverlet 撤销后不再是 openlet
(check (openlet? (coverlet (openlet (inlet 'a 1)))) => #f)


;; rootlet 不是 openlet
(check (openlet? (rootlet)) => #f)


;; 整数不是 openlet
(check (openlet? 42) => #f)


;; 字符串不是 openlet
(check (openlet? "hello") => #f)


(check-report)
