(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; generator-every
;; 测试生成器中的所有元素是否都满足谓词条件。
;;
;; 语法
;; ----
;; (generator-every pred gen)
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
;; 一旦有元素使 pred 为 #f，立即返回 #f；若所有元素均满足条件，则返回最后一个元素对应的谓词计算值（若生成器为空则返回 #t）。
;;
;; 错误处理
;; ----
;; 无

(let ((g (generator 2 4 6)))
  (check (generator-every even? g) => #t)
)

(let ((g (generator 2 3 4)))
  (check (generator-every even? g) => #f)
)

(let ((g (generator)))
  (check (generator-every even? g) => #t)
)

(check-report)
