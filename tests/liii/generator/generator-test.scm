(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; generator
;; 创建一个依次返回给定参数的生成器，耗尽后返回文件结束符 (eof-object)。
;;
;; 语法
;; ----
;; (generator arg ...)
;;
;; 参数
;; ----
;; arg ... : any
;; 生成器所依次产出的元素。
;;
;; 返回值
;; ----
;; procedure
;; 一个无参生成器过程。每次调用返回下一个元素；耗尽时返回 eof-object。
;;
;; 错误处理
;; ----
;; 无

(let ((g (generator 1 2 3)))
  (check (g) => 1)
  (check (g) => 2)
  (check (g) => 3)
  (check-true (eof-object? (g)))
  (check-true (eof-object? (g)))
)

(let ((g (generator)))
  (check-true (eof-object? (g)))
)

(check-report)
