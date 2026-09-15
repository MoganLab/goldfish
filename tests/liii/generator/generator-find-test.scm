(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; generator-find
;; 在生成器产出的序列中寻找首个满足谓词条件的元素。
;;
;; 语法
;; ----
;; (generator-find pred gen)
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
;; 首次使 pred 为真值的元素；若生成器耗尽仍未找到则返回 #f。
;;
;; 错误处理
;; ----
;; 无

(let ((g (generator 1 2 3 4)))
  (check (generator-find even? g) => 2)
)

(let ((g (generator 1 4)))
  (check (generator-find even? g) => 4)
)

(let ((g (generator 1 3 5)))
  (check (generator-find even? g) => #f)
)

(check-report)
