(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; get-output-string
;; 获取输出端口累积的字符串
;;
;; 语法
;; ----
;; (get-output-string port)
;;
;; 参数
;; ----
;; port : port?
;; 必须是由 open-output-string 创建的输出端口
;;
;; 返回值
;; -----
;; string?
;; 返回一个字符串，包含按输出顺序累积到端口的所有字符。
;; 注意：如果修改返回的字符串，其行为是未定义的。
;;
;; 错误
;; ----
;; wrong-type-arg
;; 如果 port 参数不是由 open-output-string 创建的端口，抛出错误。
(let ((port (open-output-string)))
  (display "xyz" port)
  (check (get-output-string port) => "xyz")
) ;let
(let ((port (open-input-string "ERROR")))
  (check-catch 'wrong-type-arg (get-output-string port))
) ;let
(check-report)