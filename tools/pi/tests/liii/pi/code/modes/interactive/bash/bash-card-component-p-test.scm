;; bash-card-component?
;; 判定对象是否为 Bash 命令卡片 TUI 组件实例。
;;
;; 语法
;; ----
;; (bash-card-component? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检验的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 若为 bash 卡片组件返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii pi code modes interactive bash)
) ;import

(check-set-mode! 'report-failed)

;; 1. make-bash-card 返回组件判定为 #t
(let ((card (make-bash-card "echo hello")))
  (check-true (bash-card-component? card))
) ;let

;; 2. 非 bash-card 组件判定为 #f
(check-false
  (bash-card-component? (make-component :render (lambda (s w) '())))
) ;check-false
(check-false (bash-card-component? "bash"))
(check-false (bash-card-component? #f))

(check-report)
