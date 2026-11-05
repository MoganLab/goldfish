;; %make-pi-trunc-result
;; 构造文本截断结果 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-trunc-result content truncated truncated-by total-lines total-bytes output-lines output-bytes first-line-exceeds-limit? last-line-partial?)
;;
;; 参数
;; ----
;; content : string?
;; 截断后保留的内容
;;
;; truncated : boolean?
;; 是否发生了截断
;;
;; truncated-by : string? 或 #f
;; 截断原因，如 max-lines、max-bytes
;;
;; total-lines : integer?
;; 原始内容的总行数
;;
;; total-bytes : integer?
;; 原始内容的总字节数
;;
;; output-lines : integer?
;; 实际输出的行数
;;
;; output-bytes : integer?
;; 实际输出的字节数
;;
;; first-line-exceeds-limit? : boolean?
;; 首行是否本身就超过了长度限制
;;
;; last-line-partial? : boolean?
;; 末行是否被截断为不完整内容
;;
;; 返回值
;; ----
;; pi-trunc-result?
;; 文本截断结果 record 实例。

(import (liii check) (liii pi ai tools types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-trunc-result "line1\nline2" #t "max-lines" 100 2048 2 16 #t #f))
     ) ;
  (check-true (pi-trunc-result? obj))
  (check (pi-trunc-result-content obj) => "line1\nline2")
  (check (pi-trunc-result-truncated obj) => #t)
  (check (pi-trunc-result-truncated-by obj) => "max-lines")
  (check (pi-trunc-result-total-lines obj) => 100)
  (check (pi-trunc-result-total-bytes obj) => 2048)
  (check (pi-trunc-result-output-lines obj) => 2)
  (check (pi-trunc-result-output-bytes obj) => 16)
  (check (pi-trunc-result-first-line-exceeds-limit? obj) => #t)
  (check (pi-trunc-result-last-line-partial? obj) => #f)
) ;let

(check-report)
