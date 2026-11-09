;; written
;; 按 write 语义输出对象的格式化组合子。
;;
;; 语法
;; ----
;; (written obj)
;; (written-shared obj)
;; (written-simply obj)
;;
;; 参数
;; ----
;; obj : any
;; 待输出的对象。
;;
;; 返回值
;; ----
;; format?
;; 输出时按 write 语义渲染 obj 的格式化组合子。
;;
;; 说明
;; ----
;; written 会为共享与循环结构生成 `#0=` / `#0#` datum 标签；
;; written-shared 只标注共享（非循环）结构，written-simply
;; 完全不标注共享结构。
;; `(liii format base)` 以 Goldfish 命名重导出这些函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (written "hi")) => "\"hi\"")
(check (show #f (written "hi \"bob\"")) => "\"hi \\\"bob\\\"\"")
(check (show #f (written '(1 2 3))) => "(1 2 3)")
(check (show #f (written '(1 2 . 3))) => "(1 2 . 3)")

;; 循环结构用 #0= / #0# 标注
(check
  (show #f (written (let ((ones (list 1))) (set-cdr! ones ones) ones)))
  =>
  "#0=(1 . #0#)"
) ;check

(check-report)
