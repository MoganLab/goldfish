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
;; 子进程退出码。0 → complete；-1 → timeout；-2 → aborted（ESC 中止）；其他 → error。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii string)
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

;; 2. 退出码 -2 结算为 aborted：渲染含 (aborted) 状态行
(let* ((card (make-bash-card "sleep 30"))
       (_ (bash-card-set-complete! card -2))
       (text (string-join (component-render card 80) "\n"))
      ) ;
  (check-true (string-contains? text "(aborted)"))
  (check-true (not (bash-card-running? card)))
  (check-true (not (string-contains? text "Running...")))
) ;let*

(check-report)
