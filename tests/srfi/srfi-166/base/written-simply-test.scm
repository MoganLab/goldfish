;; written-simply
;; 不标注共享结构的 write 组合子。
;;
;; 语法
;; ----
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
;; 输出对象外部表示的格式化组合子。
;;
;; 说明
;; ----
;; 输出与 write 一致，不生成任何 `#n=` / `#n#` 标签，
;; 因此循环结构会陷入无限展开，只适合已知无环的对象。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (written-simply '(1 2 3))) => "(1 2 3)")
(check (show #f (written-simply "hi")) => "\"hi\"")
(check
  (show #f (written-simply (let ((p (list 1))) (list p p))))
  =>
  "((1) (1))"
) ;check

(check-report)
