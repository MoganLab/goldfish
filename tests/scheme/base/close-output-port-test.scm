(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; close-output-port
;; 关闭输出端口。
;;
;; 语法
;; ----
;; (close-output-port port)
;;
;; 参数
;; ----
;; port : output-port?
;; 要关闭的输出端口。
;;
;; 返回值
;; ------
;; 未指定
;;
;; 说明
;; ----
;; 1. 关闭后不能再向该端口写入
;; 2. 多次关闭是安全的
(let ((p (open-output-string)))
  (close-output-port p)
  (check (output-port? p) => #t)
) ;let

(check-report)
