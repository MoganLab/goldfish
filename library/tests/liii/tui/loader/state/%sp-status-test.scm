;; %sp-status
;; 获取微调器内部记录的状态符号。
;;
;; 语法
;; ----
;; (%sp-status d)
;;
;; 参数
;; ----
;; d : %spinner-data?
;; 内部微调器状态记录。
;;
;; 返回值
;; ----
;; symbol?
;; 状态符号。

(import (scheme base)
  (liii check)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-spinner-data '("a") 0 "" "" "" 80 #t 'spinning #f #f)))
  (check (%sp-status d) => 'spinning)
) ;let

(check-report)
