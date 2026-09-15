(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; make-for-each-generator
;; 将支持遍历操作的 for-each 类过程和对应对象转换为生成器。
;;
;; 语法
;; ----
;; (make-for-each-generator for-each obj)
;;
;; 参数
;; ----
;; for-each : procedure
;; 接收两参的过程 (for-each proc obj)，用以遍历 obj 中的每个元素。
;;
;; obj : any
;; 待遍历的对象（如列表、向量或字符串等）。
;;
;; 返回值
;; ----
;; procedure
;; 一个无参生成器过程。每次调用返回 obj 的下一个遍历项，遍历完毕后返回 eof-object。
;;
;; 错误处理
;; ----
;; 无

(let ((g (make-for-each-generator for-each '(1 2 3))))
  (check (g) => 1)
  (check (g) => 2)
  (check (g) => 3)
  (check-true (eof-object? (g)))
)

(let ((g (make-for-each-generator string-for-each "ab")))
  (check (g) => #\a)
  (check (g) => #\b)
  (check-true (eof-object? (g)))
)

(check-report)
