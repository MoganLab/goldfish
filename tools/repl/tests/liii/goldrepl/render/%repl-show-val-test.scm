;; %repl-show-val
;; 把求值结果注册为会话变量并整理成输出行。
;;
;; 语法
;; ----
;; (%repl-show-val ok? result app-out!)
;;
;; 参数
;; ----
;; ok? : boolean
;; 求值是否成功。
;; result : any
;; 求值结果。
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
  (liii goldrepl render)
) ;import

(check-set-mode! 'report-failed)

;; 求值成功时输出会话变量名与结果
(let ((lines '()))
  (%repl-show-val #t 42 (lambda (l) (set! lines (append lines l))))
  (check (length lines) => 1)
  (check-true (string-contains (car lines) "$1"))
  (check-true (string-contains (car lines) "42"))
) ;let

;; 求值失败时不输出任何行
(let ((lines '()))
  (%repl-show-val #f #f (lambda (l) (set! lines (append lines l))))
  (check lines => '())
) ;let

(check-report)
