(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; reverse-vector-accumulator
;; 创建按输入顺序将元素收集为逆序向量的累加器。
;;
;; 语法
;; ----
;; (reverse-vector-accumulator)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; procedure
;; 一个单参累加器过程。传入普通元素时存入内部缓存；传入 eof-object 时按逆序返回所收集元素的向量。
;;
;; 错误处理
;; ----
;; 无

(let ((a (reverse-vector-accumulator)))
  (a 1)
  (a 2)
  (check (a (eof-object)) => '#(2 1))
)

(let ((a (reverse-vector-accumulator)))
  (check (a (eof-object)) => '#())
)

(check-report)
