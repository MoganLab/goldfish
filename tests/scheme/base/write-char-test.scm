(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; write-char
;; 向输出端口写入一个字符。
;;
;; 语法
;; ----
;; (write-char char)
;; (write-char char port)
;;
;; 参数
;; ----
;; char : char?
;; 要写入的字符。
;; port : output-port?
;; 可选，默认为当前输出端口。
;;
;; 返回值
;; ------
;; 未指定
;;
;; 说明
;; ----
;; 1. 直接写入字符，不添加引号
(let ((p (open-output-string)))
  (write-char #\a p)
  (write-char #\b p)
  (check (get-output-string p) => "ab")
) ;let

(let ((p (open-output-string)))
  (write-char #\中 p)
  (check (get-output-string p) => "中")
) ;let

(check-report)
