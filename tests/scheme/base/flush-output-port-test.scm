(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; flush-output-port
;; 刷新输出端口缓冲区。
;;
;; 语法
;; ----
;; (flush-output-port)
;; (flush-output-port port)
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
;; 1. 确保缓冲区内容已输出
;; 2. 对字符串输出端口通常无实际效果
(let ((p (open-output-string)))
  (display "test" p)
  (flush-output-port p)
  (check (get-output-string p) => "test")
) ;let

(check-report)
