;; pi-app-submit!
;; 交互式应用中提交用户输入内容（命令或对话消息）。
;;
;; 语法
;; ----
;; (pi-app-submit! app text)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; text : string
;; 待提交的用户输入文本。
;;
;; 返回值
;; ------
;; any
;; 执行消息入队或命令派发的副作用。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 提交普通文本触发 handler 执行
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (submitted #f)
       (app (make-pi-app :terminal vterm :handler (lambda (a m) (set! submitted m))))
      ) ;
  (pi-app-submit! app "hello world")
  (check submitted => "hello world")
) ;let*

;; 2. 提交 /login 路由到登录选择器而不会作为普通文本提交给 handler
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (submitted #f)
       (app (make-pi-app :terminal vterm :handler (lambda (a m) (set! submitted m))))
      ) ;
  (pi-app-submit! app "/login")
  (check submitted => #f)
  (check-true (if (pi-app-active-selector app) #t #f))
) ;let*

(check-report)
