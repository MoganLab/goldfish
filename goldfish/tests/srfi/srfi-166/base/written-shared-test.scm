;; written-shared
;; 只标注共享结构的 write 组合子。
;;
;; 语法
;; ----
;; (written-shared obj)
;;
;; 参数
;; ----
;; obj : any
;; 待输出的对象。
;;
;; 返回值
;; ----
;; format?
;; 输出带共享结构标签的格式化组合子。
;;
;; 说明
;; ----
;; 与 written 的区别是只为「被引用多次但非循环」的结构生成
;; `#n=` / `#n#` 标签，循环结构仍由 written 负责。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (written-shared '(1 2 3))) => "(1 2 3)")
(check
  (show #f (written-shared (let ((ones (list 1))) (list ones ones))))
  =>
  "(#0=(1) #0#)"
) ;check

(check-report)
