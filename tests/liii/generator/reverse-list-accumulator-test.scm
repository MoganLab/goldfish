(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; reverse-list-accumulator
;; 创建按输入顺序将元素收集为逆序列表的累加器。
;;
;; 语法
;; ----
;; (reverse-list-accumulator)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; procedure
;; 一个单参累加器过程。传入普通元素时存入内部列表；传入 eof-object 时按逆序返回所收集元素的列表。
;;
;; 错误处理
;; ----
;; 无

(let ((a (reverse-list-accumulator)))
  (a 1)
  (a 2)
  (check (a (eof-object)) => '(2 1))
)

(let ((a (reverse-list-accumulator)))
  (check (a (eof-object)) => '())
)

(check-report)
