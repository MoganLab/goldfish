(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; gappend
;; 将多个生成器首尾串联拼接为一个新的生成器。
;;
;; 语法
;; ----
;; (gappend gen ...)
;;
;; 参数
;; ----
;; gen ... : procedure
;; 待拼接的零个或多个生成器。
;;
;; 返回值
;; ----
;; procedure
;; 新的生成器过程。依次遍历并产出每个生成器的全部元素，所有生成器耗尽时返回 eof-object。
;;
;; 错误处理
;; ----
;; 无

(let ((g (gappend (generator 1 2) (generator 3) (generator))))
  (check (generator->list g) => '(1 2 3))
)

(let ((g (gappend)))
  (check-true (eof-object? (g)))
)

(let ((g (gappend (generator 1) (generator 2))))
  (check (g) => 1)
  (check (g) => 2)
  (check-true (eof-object? (g)))
)

(check-report)
