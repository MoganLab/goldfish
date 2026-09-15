(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; circular-generator
;; 创建一个无限循环重复产出给定参数序列的生成器。
;;
;; 语法
;; ----
;; (circular-generator arg ...)
;;
;; 参数
;; ----
;; arg ... : any
;; 循环产出的元素序列。
;;
;; 返回值
;; ----
;; procedure
;; 一个无参生成器过程。无限循环产出指定的元素序列。
;;
;; 错误处理
;; ----
;; 无

(let ((g (circular-generator 1 2)))
  (check (g) => 1)
  (check (g) => 2)
  (check (g) => 1)
  (check (g) => 2)
)

(let ((g (circular-generator 'a)))
  (check (g) => 'a)
  (check (g) => 'a)
)

(check-report)
