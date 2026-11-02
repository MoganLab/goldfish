;; pi-app-submit-resume!
;; 交互式应用处理 /resume 会话恢复命令提交。
;;
;; 语法
;; ----
;; (pi-app-submit-resume! app text)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; text : string
;; 用户输入的命令字符串。
;;
;; 返回值
;; ------
;; any
;; 执行打开选择器或报错提示的副作用。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 错误用法给出 Usage 提示
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (pi-app-submit-resume! app "/resume invalid-arg")
  (check-true (pair? (pi-app-chat app)))
) ;let*

(check-report)
