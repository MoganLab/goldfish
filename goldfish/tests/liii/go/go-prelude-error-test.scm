;; go-result / go-apply/source
;; worker 前导 import 失败同样通过结果 channel 回传，不绕过错误协议直接写 stderr。
;; 前导导入发生在目标函数执行之前，目标任务内的 catch 无法处理此错误。
;;
;; 语法
;; ----
;; (go-result body ...)
;; (go-apply/source src env args ch)
;;
;; 参数
;; ----
;; body ... : any
;; worker 中执行的任务体。
;;
;; src : list | symbol
;; 过程源码；env 为自由变量环境，args 为参数列表，ch 为结果 channel。
;;
;; 返回值
;; ----
;; go-result 返回结果 channel；go-apply/source 返回 unspecified。
;; 前导失败时 channel 收到 (error tag args)，目标任务不会执行。

(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; 前导 import 在外层 catch 内执行：结果 channel 必须收到 io-error。
(let ((ch (make-chan 1)))
  (go-apply/source '(lambda () (import (gfcode missing-prelude-library)) 'not-run)
    (rootlet)
    '()
    ch
  ) ;go-apply/source
  (let ((result (chan-recv! ch 5000 #f)))
    (check-true (and (pair? result) (eq? (car result) 'error)))
    (check (cadr result) => 'io-error)
  ) ;let
) ;let

;; go-result 与 go-apply/source 共用同样的前导错误语义。
(let ((ch (go-result (import (gfcode another-missing-prelude-library)) 'not-run)))
  (let ((result (chan-recv! ch 5000 #f)))
    (check-true (and (pair? result) (eq? (car result) 'error)))
    (check (cadr result) => 'io-error)
  ) ;let
) ;let

(check-report)
