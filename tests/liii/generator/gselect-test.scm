(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; gselect
;; 根据布尔值生成器，从值生成器中选取对应位置为真值的元素。
;;
;; 语法
;; ----
;; (gselect value-gen truth-gen)
;;
;; 参数
;; ----
;; value-gen : procedure
;; 提供值的生成器。
;;
;; truth-gen : procedure
;; 提供布尔条件值的生成器。
;;
;; 返回值
;; ----
;; procedure
;; 新生成器过程。仅当 truth-gen 对应项为真时产出 value-gen 对应的元素。任一生成器耗尽时停止。
;;
;; 错误处理
;; ----
;; 无

(let ((g (gselect (generator 1 2 3) (generator #t #f #t))))
  (check (generator->list g) => '(1 3))
)

(let ((g (gselect (generator 1 2 3) (generator #f #t))))
  (check (generator->list g) => '(2))
)

(let ((g (gselect (generator 1 2 3) (generator #f #t #t #t))))
  (check (generator->list g) => '(2 3))
)

(check-report)
