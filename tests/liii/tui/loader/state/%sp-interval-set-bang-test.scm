;; %sp-interval-set!
;; 设置微调器内部记录的刷新间隔。
;;
;; 语法
;; ----
;; (%sp-interval-set! d interval)
;;
;; 参数
;; ----
;; d : %spinner-data?
;; 内部微调器状态记录。
;;
;; interval : integer?
;; 刷新间隔。
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
  (%sp-interval-set! d 50)
  (check (%sp-interval d) => 50)
) ;let

(check-report)
