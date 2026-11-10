;; make-help-box
;; 构造操作说明侧栏组件
;;
;; 语法
;; ----
;; (make-help-box)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; component?\n带按键说明的边框组件

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui snake view)
  (liii tui snake style)
) ;import

(check-set-mode! 'report-failed)

(with-color-mode #f
  (lambda ()
    (let* ((help-box (make-help-box))
           (help-lines (component-render help-box 25))
          ) ;
      (check (component? help-box) => #t)
      (check (pair? help-lines) => #t)
    ) ;let*
  ) ;lambda
) ;with-color-mode

(with-color-mode #t
  (lambda ()
    (check (component? (make-help-box)) => #t)
  ) ;lambda
) ;with-color-mode

(check-report)
