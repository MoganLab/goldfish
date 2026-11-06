;; %sp-status-set!
;; 设置微调器内部记录的状态符号。
;;
;; 语法
;; ----
;; (%sp-status-set! d status)
;;
;; 参数
;; ----
;; d : %spinner-data?
;; 内部微调器状态记录。
;;
;; status : symbol?
;; 状态符号。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base)
  (liii check)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-spinner-data '("a") 0 "" "" "" 80 #t 'spinning #f #f)))
  (%sp-status-set! d 'success)
  (check (%sp-status d) => 'success)
) ;let

(check-report)
