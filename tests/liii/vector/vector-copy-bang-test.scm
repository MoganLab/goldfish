(import (liii check)
        (liii vector)
) ;import

(check-set-mode! 'report-failed)

;; vector-copy!
;; 将源向量元素复制到目标向量。
;;
;; 语法
;; ----
;; (vector-copy! to at from)
;; (vector-copy! to at from start)
;; (vector-copy! to at from start end)
;;
;; 参数
;; ----
;; to : vector?
;; 目标向量。
;;
;; at : integer?
;; 复制到目标向量的起始位置。
;;
;; from : vector?
;; 源向量。
;;
;; start : integer? 可选
;; 源向量起始位置（包含），默认为0。
;;
;; end : integer? 可选
;; 源向量结束位置（不包含），默认为源向量长度。
;;
;; 返回值
;; ----
;; unspecified
;; 主要关注目标向量被复制后的结果。
;;
;; 注意
;; ----
;; 目标区间必须有足够空间容纳复制内容。
;;
;; 示例
;; ----
;; (vector-copy! b 0 a 1) => 从a的索引1开始复制到b
;;
;; 错误处理
;; ----
;; out-of-range 当at/start/end越界、start大于end或目标空间不足时

(define a (vector "a0" "a1" "a2" "a3" "a4"))
(define b (vector "b0" "b1" "b2" "b3" "b4"))

(check-catch 'out-of-range (vector-copy! b -1 a))
(check-catch 'out-of-range (vector-copy! b 0 a -1))
(check-catch 'out-of-range (vector-copy! b 0 a 6))
(check-catch 'out-of-range (vector-copy! b 0 a 0 6))
(check-catch 'out-of-range (vector-copy! b 0 a 2 1))
(check-catch 'out-of-range (vector-copy! b 6 a))
(check-catch 'out-of-range (vector-copy! b 1 a))

(define a (vector "a0" "a1" "a2" "a3" "a4"))
(define b (vector "b0" "b1" "b2" "b3" "b4"))
(vector-copy! b 0 a 1)
(check b => #("a1" "a2" "a3" "a4" "b4"))

(define a (vector "a0" "a1" "a2" "a3" "a4"))
(define b (vector "b0" "b1" "b2" "b3" "b4"))
(vector-copy! b 0 a 0 5)
(check b => #("a0" "a1" "a2" "a3" "a4"))

(check-report)
