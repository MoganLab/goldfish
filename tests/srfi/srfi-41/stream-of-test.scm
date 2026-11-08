;; stream-of
;; 流推导式： comprehension 语法构造流。
;;
;; 语法
;; ----
;; (stream-of expr clause ...)
;;
;; 参数
;; ----
;; expr : 生成元素的表达式
;;
;; clause : (var in strm) | (var is exp) | 谓词表达式
;; 返回值
;; ----
;; stream?
;; 按 clause 约束求值 expr 产出的流。
;;
;; 说明
;; ----
;; in 迭代流元素；is 绑定；裸表达式作过滤条件。
;; 谓词终止依赖有限流；无限流配谓词会不终止。

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check
  (stream->list
    (stream-of (+ y 6)
      (x in (stream-range 1 6))
      (odd? x)
      (y is (* x x))))
  => '(7 15 31))
(check
  (stream->list
    (stream-of (* x y)
      (x in (stream-range 1 4))
      (y in (stream-range 1 5))))
  => '(1 2 3 4 2 4 6 8 3 6 9 12))
(check (stream-car (stream-of 1)) => 1)
(check (stream->list (stream-of (* x x) (x in (stream 1 2 3)))) => '(1 4 9))

(check-report)
