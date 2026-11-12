;; bash-card-running?
;; 判断 Bash 命令卡片是否仍在运行（含已派发尚未开始执行的排队命令）。
;;
;; 语法
;; ----
;; (bash-card-running? card)
;;
;; 参数
;; ----
;; card : component?
;; Bash 命令卡片组件。
;;
;; 返回值
;; ----
;; boolean?
;; 卡片状态为 running 时 #t；ESC 中止门控（对齐上游 isBashRunning）的数据源。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive bash)
) ;import

(check-set-mode! 'report-failed)

;; 1. 新建卡片为 running（命令派发即置 running）
(let ((card (make-bash-card "echo hi")))
  (check-true (bash-card-running? card))
) ;let

;; 2. 结算后不再 running：0 / -1 / -2 三种终态都不算运行中
(let ((card (make-bash-card "echo hi")))
  (bash-card-set-complete! card 0)
  (check-true (not (bash-card-running? card)))
) ;let

(let ((card (make-bash-card "sleep 5")))
  (bash-card-set-complete! card -1)
  (check-true (not (bash-card-running? card)))
) ;let

(let ((card (make-bash-card "sleep 5")))
  (bash-card-set-complete! card -2)
  (check-true (not (bash-card-running? card)))
) ;let

(check-report)
