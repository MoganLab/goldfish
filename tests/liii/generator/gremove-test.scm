(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; gremove
;; 过滤生成器，移除满足谓词条件的元素（保留使谓词为假值的元素）。
;;
;; 语法
;; ----
;; (gremove pred gen)
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
;; 过滤后的生成器过程。依次产出使 pred 返回 #f 的元素。
;;
;; 错误处理
;; ----
;; 无

(let ((g (gremove even? (generator 1 2 3 4))))
  (check (generator->list g) => '(1 3))
)

(let ((g (gremove (lambda (x) (> x 2)) (generator 1 2 3 4))))
  (check (generator->list g) => '(1 2))
)

(check-report)
