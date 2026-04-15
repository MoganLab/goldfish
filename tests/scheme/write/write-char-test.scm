(import (liii check) (scheme write))
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
;; 要输出的字符。
;;
;; port : output-port? (可选)
;; 输出端口。省略时，写入当前输出端口。
;;
;; 返回值
;; ----
;; unspecified
;; 主要用于副作用输出。
;;
;; 错误处理
;; ----
;; wrong-type-arg
;; 当第一个参数不是字符时抛出。
(define (capture-output thunk)
  (let ((port (open-output-string)))
    (thunk port)
    (get-output-string port)
  ) ;let
) ;define
(check-true (procedure? write-char))
(check (capture-output (lambda (port) (write-char #\A port)))
  =>
  "A"
) ;check
(check (capture-output (lambda (port)
                         (write-char #\space port)
                         (write-char #\B port)
                       ) ;lambda
       ) ;capture-output
  =>
  " B"
) ;check
(check-catch 'wrong-type-arg
  (let ((port (open-output-string)))
    (write-char 1 port)
  ) ;let
) ;check-catch
(check-report)