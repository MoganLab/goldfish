(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; open-output-string
;; 创建一个字符串输出端口用于累积字符
;;
;; 语法
;; ----
;; (open-output-string)
;;
;; 返回值
;; -----
;; port
;; 返回一个新的文本输出端口，所有写入该端口的字符会被累积，
;; 可通过 get-output-string 函数获取累积的字符串。
;; empty
(let ((port (open-output-string)))
  (check (get-output-string port) => "")
) ;let
(let ((port (open-output-string)))
  (display "abc" port)
  (check (get-output-string port)
    =>
    "abc"
  ) ;check
) ;let
(let ((port (open-output-string)))
  (display "λμ" port)
  (check (get-output-string port)
    =>
    "λμ"
  ) ;check
) ;let
(check-report)