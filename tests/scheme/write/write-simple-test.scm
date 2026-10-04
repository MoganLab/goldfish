(import (liii check) (scheme write))
(check-set-mode! 'report-failed)
;; write-simple
;; 输出不带共享标签的可读表示；循环结构明确报错。
;;
;; 语法
;; ----
;; (write-simple obj)
;; (write-simple obj port)
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
;; 共享的非循环结构按树展开，不生成 datum label。

(define (capture-output thunk)
  (let ((port (open-output-string)))
    (thunk port)
    (get-output-string port)
  ) ;let
) ;define
(check-true (procedure? write-simple))
(check (capture-output (lambda (port) (write-simple '(a b) port))) => "(a b)")
(check (capture-output (lambda (port) (write-simple "goldfish" port)))
  =>
  "\"goldfish\""
) ;check
(check (capture-output (lambda (port) (write-simple 123 port))) => "123")
(check-report)
