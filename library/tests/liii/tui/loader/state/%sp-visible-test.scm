;; %sp-visible
;; 获取微调器内部记录的可见性。
;;
;; 语法
;; ----
;; (%sp-visible d)
;;
;; 参数
;; ----
;; d : %spinner-data?
;; 内部微调器状态记录。
;;
;; 返回值
;; ----
;; boolean?
;; 是否可见。

(import (scheme base)
  (liii check)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-spinner-data '("a") 0 "" "" "" 80 #t 'spinning #f #f)))
  (check-true (%sp-visible d))
) ;let

(check-report)
