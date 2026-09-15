(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; make-accumulator
;; 创建自定义累加器过程。
;;
;; 语法
;; ----
;; (make-accumulator kons knil finalize)
;;
;; 参数
;; ----
;; kons : procedure
;; 接收两参的过程 (kons obj state)，用于将新元素累加到当前状态。
;;
;; knil : any
;; 初始状态值。
;;
;; finalize : procedure
;; 单参过程 (finalize state)，在遇到 eof-object 时调用并返回最终计算结果。
;;
;; 返回值
;; ----
;; procedure
;; 一个单参累加器过程 (accum obj)。传入普通元素时更新内部状态；传入 eof-object 时返回经过 finalize 处理的结果。
;;
;; 错误处理
;; ----
;; 无

(let ((a (make-accumulator + 0 (lambda (x) x))))
  (a 1)
  (check (a (eof-object)) => 1)
  (a 2)
  (check (a (eof-object)) => 3)
  (a 3)
  (check (a (eof-object)) => 6)
)

(let* ((res #f)
       (accum (make-accumulator * 1 (lambda (state) (set! res state)))))
  (accum 2)
  (check res => #f)
  (accum 3)
  (accum (eof-object))
  (check res => 6)
)

(check-report)
