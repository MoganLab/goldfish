(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; generator-unfold
;; 配合展开器函数 unfold 消费生成器并构造集合对象。
;;
;; 语法
;; ----
;; (generator-unfold gen unfold arg ...)
;;
;; 参数
;; ----
;; gen : procedure
;; 输入生成器。
;;
;; unfold : procedure
;; 类似于 SRFI-1 unfold 的展开构造函数。
;;
;; arg ... : any (可选)
;; 传递给 unfold 的附加参数。
;;
;; 返回值
;; ----
;; any
;; 由 unfold 展开生成的集合结构。
;;
;; 错误处理
;; ----
;; 无

(define (simple-unfold p f g seed)
  (if (p seed) '() (cons (f seed) (simple-unfold p f g (g seed))))
)

(let ((g (generator 1 2 3)))
  (check (generator-unfold g simple-unfold) => '(1 2 3))
)

(let ((g (make-for-each-generator string-for-each "abc")))
  (check (list->string (generator-unfold g simple-unfold)) => "abc")
)

(check-report)
