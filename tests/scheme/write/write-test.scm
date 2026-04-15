(import (liii check) (scheme write))
(check-set-mode! 'report-failed)
;; write
;; 将对象按可读回的 Scheme 表示写入输出端口。
;;
;; 语法
;; ----
;; (write obj)
;; (write obj port)
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
;; 1. `write` 面向"可读回"的文本表示。
;; 2. 字符串会带双引号。
;; 3. 列表、符号等按 Scheme 语法形式输出。
(define (capture-output thunk)
  (let ((port (open-output-string)))
    (thunk port)
    (get-output-string port)
  ) ;let
) ;define
(check-true (procedure? write))
(check (capture-output (lambda (port) (write '(1 2 3) port))
       ) ;capture-output
  =>
  "(1 2 3)"
) ;check
(check (capture-output (lambda (port) (write "goldfish" port))
       ) ;capture-output
  =>
  "\"goldfish\""
) ;check
(check (capture-output (lambda (port)
                         (write 'hello-world port)
                       ) ;lambda
       ) ;capture-output
  =>
  "hello-world"
) ;check
(check (capture-output (lambda (port) (write 42 port))
       ) ;capture-output
  =>
  "42"
) ;check
(check-report)
