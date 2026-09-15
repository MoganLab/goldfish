(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; vector-accumulator
;; 创建按输入顺序将元素收集为向量的累加器。
;;
;; 语法
;; ----
;; (vector-accumulator)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; procedure
;; 一个单参累加器过程。传入普通元素时存入内部缓存；传入 eof-object 时按接收顺序返回所收集元素的向量。
;;
;; 说明
;; ----
;; 若需就地填充已有向量而非分配新向量，参见：
;;   gf doc "vector-accumulator!"
;;
;; 错误处理
;; ----
;; 无

(let ((a (vector-accumulator)))
  (a 1)
  (a 2)
  (check (a (eof-object)) => '#(1 2))
) ;let

(let ((a (vector-accumulator)))
  (check (a (eof-object)) => '#())
) ;let

(check-report)
