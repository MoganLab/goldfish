;; bash-card-expanded?
;; 查询 Bash 命令卡片当前是否处于展开状态。
;;
;; 语法
;; ----
;; (bash-card-expanded? card)
;;
;; 参数
;; ----
;; card : component?
;; Bash 命令卡片组件。
;;
;; 返回值
;; ----
;; boolean?
;; 展开返回 #t，折叠返回 #f。

(import (scheme base) (liii check) (liii pi code modes interactive bash))

(check-set-mode! 'report-failed)

;; 1. 默认折叠状态为 #f
(let ((card (make-bash-card "ls")))
  (check-false (bash-card-expanded? card))
  ;; 2. 展开后返回 #t
  (bash-card-set-expanded! card #t)
  (check-true (bash-card-expanded? card))
) ;let

(check-report)
