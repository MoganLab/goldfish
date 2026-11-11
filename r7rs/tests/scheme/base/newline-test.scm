;; newline
;; 向输出端口写入一个换行符。
;;
;;
;; 语法
;; ----
;; (newline)
;; (newline port)
;;
;; 参数
;; ----
;; port : output-port? (可选)
;; 输出端口。省略时，写入当前输出端口。
;;
;; 返回值
;; ----
;; unspecified
;; 主要用于副作用输出。
;;
;; 描述
;; ----
;; `newline` 常用于组织多行输出，与 `display` 或 `write` 搭配使用。

(import (liii check) (scheme base))

(check-set-mode! 'report-failed)

(define (capture-output thunk)
  (let ((port (open-output-string)))
    (thunk port)
    (get-output-string port)
  ) ;let
) ;define
(check-true (procedure? newline))
(check (capture-output (lambda (port) (newline port))) => "\n")
(check (capture-output (lambda (port) (display "a" port) (newline port) (display "b" port))
       ) ;capture-output
  =>
  "a\nb"
) ;check

;; 错误处理：二进制端口不是文本端口
(let ((p (open-output-bytevector)))
  (check-catch 'type-error (newline p))
) ;let

;; 错误处理：已关闭的端口
(let ((p (open-output-string)))
  (close-output-port p)
  (check-catch 'type-error (newline p))
) ;let

(check-report)
