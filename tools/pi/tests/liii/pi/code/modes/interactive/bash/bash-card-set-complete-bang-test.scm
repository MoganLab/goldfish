;; bash-card-set-complete!
;; 标记 Bash 命令卡片执行完成并记录退出码。
;;
;; 语法
;; ----
;; (bash-card-set-complete! card exit-code)
;;
;; 参数
;; ----
;; card : component?
;; Bash 命令卡片组件。
;; exit-code : integer?
;; 子进程退出码。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive bash)
) ;import

(check-set-mode! 'report-failed)

;; 1. 标记完成并更新退出码状态
(let* ((card (make-bash-card "pwd"))
       (_ (bash-card-set-complete! card 0))
       (lines (component-render card 80))
      ) ;
  (check-true (pair? lines))
) ;let*

(check-report)
