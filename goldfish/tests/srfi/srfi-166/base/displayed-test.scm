;; displayed
;; 按 display 语义输出对象的格式化组合子。
;;
;; 语法
;; ----
;; (displayed x)
;;
;; 参数
;; ----
;; x : any
;; 待输出的对象。
;;
;; 返回值
;; ----
;; format?
;; 输出时按 display 语义渲染 x 的格式化组合子。
;;
;; 说明
;; ----
;; 字符串与字符原样输出；computation 会被调用；
;; 其余对象退化为按 written 语义输出。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (displayed "hi")) => "hi")
(check (show #f (displayed #\a)) => "a")
(check (show #f (displayed 123)) => "123")
(check (show #f (displayed '(1 2))) => "(1 2)")
(check (show #f (displayed (each "x" "y"))) => "xy")

(check-report)
