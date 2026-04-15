(import (liii check) (scheme write))
(check-set-mode! 'report-failed)
;; newline
;; 向输出端口写入一个换行符。
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
(define (capture-output thunk)
  (let ((port (open-output-string)))
    (thunk port)
    (get-output-string port)
  ) ;let
) ;define
(check-true (procedure? newline))
(check (capture-output (lambda (port) (newline port))
       ) ;capture-output
  =>
  "\n"
) ;check
(check (capture-output (lambda (port)
                         (display "a" port)
                         (newline port)
                         (display "b" port)
                       ) ;lambda
       ) ;capture-output
  =>
  "a\nb"
) ;check
(check-report)
