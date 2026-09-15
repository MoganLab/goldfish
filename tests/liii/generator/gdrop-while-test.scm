(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; gdrop-while
;; 跳过生成器开始部分连续满足谓词条件的元素，从首个不满足条件的元素开始产出剩余所有元素。
;;
;; 语法
;; ----
;; (gdrop-while pred gen)
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
;; procedure
;; 新生成器过程。跳过开头满足 pred 的元素后，产出剩余的所有后续元素。
;;
;; 错误处理
;; ----
;; 无

(let ((g (gdrop-while odd? (generator 1 3 4 5))))
  (check (generator->list g) => '(4 5))
)

(let ((g (gdrop-while (lambda (x) (< x 10)) (generator 1 2 3))))
  (check (generator->list g) => '())
)

(check-report)
