(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; generator-map->list
;; 对一个或多个生成器产出的元素应用映射过程，并将结果收集为列表。
;;
;; 语法
;; ----
;; (generator-map->list proc gen ...)
;;
;; 参数
;; ----
;; proc : procedure
;; 映射过程，其参数个数应与生成器的数量相等。
;;
;; gen ... : procedure
;; 一个或多个输入生成器。
;;
;; 返回值
;; ----
;; list
;; 映射结果构成的列表。当任意输入生成器耗尽时停止。
;;
;; 错误处理
;; ----
;; 无

(let ((g (generator 1 2 3)))
  (check (generator-map->list (lambda (x) (+ x 1)) g) => '(2 3 4))
)

(let ((g1 (generator 1 2 3))
      (g2 (generator 10 20)))
  (check (generator-map->list + g1 g2) => '(11 22))
)

(check-report)
