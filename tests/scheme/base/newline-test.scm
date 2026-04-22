(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; newline
;; 向输出端口写入换行符。
;;
;; 语法
;; ----
;; (newline)
;; (newline port)
;;
;; 参数
;; ----
;; port : output-port?
;; 可选，默认为当前输出端口。
;;
;; 返回值
;; ------
;; 未指定
;;
;; 说明
;; ----
;; 1. 写入系统相关的行结束符
;; 2. 常用于格式化输出
(let ((p (open-output-string)))
  (newline p)
  (check (string-length (get-output-string p)) => 1)
) ;let

(check-report)
