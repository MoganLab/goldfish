(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; generator-any
;; 测试生成器中是否存在至少一个满足谓词条件的元素。
;;
;; 语法
;; ----
;; (generator-any pred gen)
;;
;; 参数
;; ----
;; pred : procedure
;; 单参谓词过程。
;;
;; gen : procedure
;; 输入生成器。
;;
;; 返回值
;; ----
;; any 或 #f
;; 一旦某个元素使 pred 返回真值，立即返回该值；若生成器耗尽仍无元素满足则返回 #f。
;;
;; 错误处理
;; ----
;; 无

(let ((g (generator 1 3 5)))
  (check (generator-any even? g) => #f)
)

(let ((g (generator 1 4 5)))
  (check (generator-any even? g) => #t)
)

(let ((g (generator 1 2 3)))
  (check (generator-any (lambda (x) (and (even? x) 'found)) g) => 'found)
)

(check-report)
