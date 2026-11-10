;; %repl-eval-and-show!
;; 求值代码并把输出、错误与结果输出到界面。
;;
;; 语法
;; ----
;; (%repl-eval-and-show! code app-out!)
;;
;; 参数
;; ----
;; code : string
;; 待求值的代码文本。
;; app-out! : procedure
;; 输出一行文本的回调，接收文本行列表。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未定义，函数通过回调输出文本行。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldrepl loop)
) ;import

(check-set-mode! 'report-failed)

;; 求值成功时输出会话变量与结果
(let ((lines '()))
  (%repl-eval-and-show! "(+ 1 2)" (lambda (l) (set! lines (append lines l))))
  (check-true (pair? lines))
  (check-true (any (lambda (l) (string-contains l "$1")) lines))
  (check-true (any (lambda (l) (string-contains l "3")) lines))
) ;let

;; 求值失败时输出错误信息
(let ((lines '()))
  (%repl-eval-and-show! "(/ 1 0)" (lambda (l) (set! lines (append lines l))))
  (check-true (pair? lines))
  (check-true (any (lambda (l) (string-contains l "\x1b;[31m")) lines))
) ;let

;; 求值产生的标准输出也会被输出
(let ((lines '()))
  (%repl-eval-and-show! "(display \"hello\")"
    (lambda (l) (set! lines (append lines l)))
  ) ;%repl-eval-and-show!
  (check-true (any (lambda (l) (string-contains l "hello")) lines))
) ;let

(check-report)
