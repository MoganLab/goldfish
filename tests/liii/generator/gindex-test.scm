(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; gindex
;; 根据单调递增的非负整数索引生成器，从值生成器中选取对应位置的元素。
;;
;; 语法
;; ----
;; (gindex value-gen index-gen)
;;
;; 参数
;; ----
;; value-gen : procedure
;; 提供值的生成器。
;;
;; index-gen : procedure
;; 提供非负整数索引的生成器，索引必须严格递增。
;;
;; 返回值
;; ----
;; procedure
;; 新生成器过程。按索引生成器指定的下标位置依次产出 value-gen 对应位置的元素。
;;
;; 错误处理
;; ----
;; 无

(let ((g (gindex (generator 'a 'b 'c 'd) (generator 1 2))))
  (check (generator->list g) => '(b c))
)

(let ((g (gindex (generator 10 20 30 40 50) (generator 0 3))))
  (check (generator->list g) => '(10 40))
)

(check-report)
