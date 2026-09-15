(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; gfilter
;; 过滤生成器，仅保留满足谓词条件的元素。
;;
;; 语法
;; ----
;; (gfilter pred gen)
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
;; 过滤后的生成器过程。依次产出使 pred 返回真值的元素。
;;
;; 错误处理
;; ----
;; 无

(let ((g (gfilter even? (generator 1 2 3 4))))
  (check (g) => 2)
  (check (g) => 4)
  (check-true (eof-object? (g)))
)

(let ((g (gfilter (lambda (x) (> x 5)) (generator 1 2 3))))
  (check-true (eof-object? (g)))
)

(check-report)
