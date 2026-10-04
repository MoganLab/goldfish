(import (liii check) (scheme write))
(check-set-mode! 'report-failed)
;; write-shared
;; 使用 datum label 保留共享及循环结构。
;;
;; 语法
;; ----
;; (write-shared obj)
;; (write-shared obj port)
;;
;; 参数
;; ----
;; obj : any
;; 要输出的对象。
;;
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
;; 非共享结构的输出与 write 一致。

(define (capture-output thunk)
  (let ((port (open-output-string)))
    (thunk port)
    (get-output-string port)
  ) ;let
) ;define
(check-true (procedure? write-shared))
(check (capture-output (lambda (port) (write-shared '(a b) port))) => "(a b)")
(check (capture-output (lambda (port) (write-shared "goldfish" port)))
  =>
  "\"goldfish\""
) ;check
(check (capture-output (lambda (port) (write-shared 456 port))) => "456")
(check-report)
