(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; gflatten
;; 将产出列表的生成器扁平化，逐个产出子列表中的元素。
;;
;; 语法
;; ----
;; (gflatten gen)
;;
;; 参数
;; ----
;; gen : procedure
;; 其产出值必须为列表的生成器。
;;
;; 返回值
;; ----
;; procedure
;; 扁平化后的生成器过程。依次产出各子列表中的单个元素。
;;
;; 错误处理
;; ----
;; 无

(let ((g (gflatten (generator (list 1 2) (list 3)))))
  (check (generator->list g) => '(1 2 3))
)

(let ((g (gflatten (generator '(1) '(2 3) '(4 5 6)))))
  (check (generator->list g) => '(1 2 3 4 5 6))
)

(let ((g (gflatten (generator))))
  (check (generator->list g) => '())
)

(check-report)
