(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; write-string
;; 向输出端口写入字符串。
;;
;; 语法
;; ----
;; (write-string string)
;; (write-string string port)
;; (write-string string port start)
;; (write-string string port start end)
;;
;; 参数
;; ----
;; string : string?
;; 要写入的字符串。
;; port : output-port?
;; 可选，默认为当前输出端口。
;; start : exact-nonnegative-integer?
;; 可选，起始索引。
;; end : exact-nonnegative-integer?
;; 可选，结束索引。
;;
;; 返回值
;; ------
;; 未指定
;;
;; 说明
;; ----
;; 1. 默认写入整个字符串
;; 2. 可通过 start/end 指定子串
(let ((p (open-output-string)))
  (write-string "hello" p)
  (check (get-output-string p) => "hello")
) ;let
(let ((p (open-output-string)))
  (write-string "hello" p 1)
  (check (get-output-string p) => "ello")
) ;let
(let ((p (open-output-string)))
  (write-string "hello" p 1 3)
  (check (get-output-string p) => "el")
) ;let

(check-report)
