(import (liii check) (scheme write))
(check-set-mode! 'report-failed)
;; write-shared
;; 在当前实现中，提供与 `write` 一致的兼容输出行为。
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
;; 当前底层没有独立的 `write-shared` 原生过程，因此这里验证它与 `write`
;; 保持一致的现有兼容行为。
(define (capture-output thunk)
  (let ((port (open-output-string)))
    (thunk port)
    (get-output-string port)
  ) ;let
) ;define
(check-true (procedure? write-shared))
(check (capture-output (lambda (port) (write-shared '(a b) port)))
  =>
  "(a b)"
) ;check
(check (capture-output (lambda (port) (write-shared "goldfish" port))
       ) ;capture-output
  =>
  "\"goldfish\""
) ;check
(check (capture-output (lambda (port) (write-shared 456 port)))
  =>
  "456"
) ;check
(check-report)