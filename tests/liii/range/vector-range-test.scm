(import (liii check)
        (liii range)
) ;import

(check-set-mode! 'report-failed)

;; vector-range
;; 从向量创建 range。
;;
;; 语法
;; ----
;; (vector-range vec)
;;
;; 参数
;; ----
;; vec : vector
;; 源向量。
;;
;; 返回值
;; ----
;; range
;; 包含向量元素的 range 对象。
;;
;; 注意
;; ----
;; 创建的 range 与源向量共享元素。
;;
;; 示例
;; ----
;; (vector-range #(a b c d e)) => 包含 a,b,c,d,e 的 range
;;
;; 错误处理
;; ----
;; 无

(let ((r (vector-range #(a b c d e))))
  (check (range-length r) => 5)
  (check (range-ref r 0) => 'a)
  (check (range-ref r 4) => 'e)
) ;let

(check-report)
