(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; gstate-filter
;; 带有内部累积状态的生成器过滤器。
;;
;; 语法
;; ----
;; (gstate-filter proc seed gen)
;;
;; 参数
;; ----
;; proc : procedure
;; 接收 (proc item state) 并返回两个值：(values keep? new-state)。若 keep? 为真则保留当前 item。
;;
;; seed : any
;; 初始状态值。
;;
;; gen : procedure
;; 输入生成器。
;;
;; 返回值
;; ----
;; procedure
;; 新生成器过程。依次产出通过状态过滤的元素。
;;
;; 错误处理
;; ----
;; 无

(let ((g (gstate-filter (lambda (x s) (values (odd? x) s)) 0 (generator 1 2 3))))
  (check (generator->list g) => '(1 3))
)

;; 用状态过滤前 2 个元素
(let ((g (gstate-filter (lambda (x count) (values (< count 2) (+ count 1))) 0 (generator 'a 'b 'c 'd))))
  (check (generator->list g) => '(a b))
)

(check-report)
