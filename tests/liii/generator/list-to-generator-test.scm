(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; list->generator
;; 将列表转换为依次产出列表元素的生成器。
;;
;; 语法
;; ----
;; (list->generator lst)
;;
;; 参数
;; ----
;; lst : list
;; 待转换的列表。
;;
;; 返回值
;; ----
;; procedure
;; 一个无参生成器过程。依次返回 lst 中的各元素，耗尽时返回 eof-object。
;;
;; 错误处理
;; ----
;; 无

(let ((g (list->generator '(7 8))))
  (check (g) => 7)
  (check (g) => 8)
  (check-true (eof-object? (g)))
)

(let ((g (list->generator '())))
  (check-true (eof-object? (g)))
)

(check-report)
