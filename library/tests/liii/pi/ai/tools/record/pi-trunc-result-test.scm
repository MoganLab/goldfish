;; pi-trunc-result
;; 构造文本截断结果 record 对象。
;;
;; 语法
;; ----
;; (pi-trunc-result content truncated)
;; (pi-trunc-result content truncated truncated-by total-lines total-bytes output-lines output-bytes first-line-exceeds-limit? last-line-partial?)
;;
;; 参数
;; ----
;; content : string?
;; 截断后的内容文本。
;;
;; truncated : boolean?
;; 是否发生了截断。
;;
;; truncated-by : symbol?
;; 可选，截断依据类型（如 'lines 或 'bytes）。
;;
;; total-lines : integer?
;; 可选，原始总行数。
;;
;; total-bytes : integer?
;; 可选，原始总字节数。
;;
;; output-lines : integer?
;; 可选，截断后保留行数。
;;
;; output-bytes : integer?
;; 可选，截断后保留字节数。
;;
;; first-line-exceeds-limit? : boolean?
;; 可选，首行是否已超出限制。
;;
;; last-line-partial? : boolean?
;; 可选，末行是否被部分截断。
;;
;; 返回值
;; ----
;; pi-trunc-result?
;; 文本截断结果 record 实例。

(import (liii check) (liii pi ai tools record))

(check-set-mode! 'report-failed)

(let ((tr (pi-trunc-result "some text" #t 'lines 100 5000 50 2500 #f #f)))
  (check-true (pi-trunc-result? tr))
  (check (pi-trunc-result-content tr) => "some text")
  (check-true (pi-trunc-result-truncated tr))
  (check (pi-trunc-result-total-lines tr) => 100)
) ;let

(check-report)
