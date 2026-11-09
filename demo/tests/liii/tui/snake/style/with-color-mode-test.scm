;; with-color-mode
;; 在指定颜色模式下执行 thunk，退出后自动恢复原状态
;;
;; 语法
;; ----
;; (with-color-mode enabled? thunk)
;;
;; 参数
;; ----
;; enabled? : boolean?\n执行期间的颜色开关；thunk : procedure?\n待执行过程
;;
;; 返回值
;; ------
;; any\nthunk 的返回值

(import (scheme base)
  (liii check)
  (liii tui snake style)
) ;import

(check-set-mode! 'report-failed)

;; 关色模式下样式直接返回原文
(with-color-mode #f
  (lambda ()
    (check (style-enabled?) => #f)
    (check (style-head "@") => "@")
    (check (style-title " STATS ") => " STATS ")
  ) ;lambda
) ;with-color-mode

;; with-color-mode 退出后自动恢复原状态
(style-enable!)
(with-color-mode #f (lambda () (check (style-enabled?) => #f)))
(check (style-enabled?) => #t)

;; 返回 thunk 的值
(check (with-color-mode #t (lambda () 'ok)) => 'ok)
(check-catch 'type-error (with-color-mode #t "not-thunk"))

(check-report)
