;; %sp-interval
;; 获取微调器内部记录的刷新间隔。
;;
;; 语法
;; ----
;; (%sp-interval d)
;;
;; 参数
;; ----
;; d : %spinner-data?
;; 内部微调器状态记录。
;;
;; 返回值
;; ----
;; integer?
;; 刷新间隔（毫秒）。

(import (scheme base)
  (liii check)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-spinner-data '("a") 0 "" "" "" 120 #t 'spinning #f #f)))
  (check (%sp-interval d) => 120)
) ;let

(check-report)
