(import (liii check)
        (srfi srfi-166 base))

(check-set-mode! 'report-failed)

;; written
;; 按 write 语义输出对象的组合子，并自动检测循环引用结构（使用 #0= 和 #0# datum 标签）。
;;
;; 语法
;; ----
;; (written obj)
;; (written-shared obj)
;; (written-simply obj)
;;
;; 示例
;; ----
;; (show #f (written "hi")) => "\"hi\""
;; (show #f (written '(1 2 3))) => "(1 2 3)"
;;
;; 另见
;; ----
;; (liii format base) 同名再导出本函数，日常使用更推荐导入该库：
;;   (import (liii format base))
;;   (show #f (written '(1 2 3)))

(check (show #f (written "hi")) => "\"hi\"")
(check (show #f (written "hi \"bob\"")) => "\"hi \\\"bob\\\"\"")
(check (show #f (written '(1 2 3))) => "(1 2 3)")
(check (show #f (written '(1 2 . 3))) => "(1 2 . 3)")

;; 循环引用与共享结构
(check (show #f (written (let ((ones (list 1))) (set-cdr! ones ones) ones)))
       => "#0=(1 . #0#)")
(check (show #f (written-shared (let ((ones (list 1))) (list ones ones))))
       => "(#0=(1) #0#)")
(check (show #f (written-simply '(1 2 3))) => "(1 2 3)")

(check-report)
