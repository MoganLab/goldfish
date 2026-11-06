;; tzif-transition-time
;; 读取 tzif-transition 记录的时刻字段。
;;
;; 语法
;; ----
;; (tzif-transition-time tr)
;;
;; 参数
;; ----
;; tr : tzif-transition? 转移记录。
;;
;; 返回值
;; ----
;; integer? 转移发生的时刻。

(import (liii check) (liii date-time internal tzif))

(check-set-mode! 'report-failed)

(define tr (tzif-transition 100 3600 #t "EDT" #f #f))

(check (tzif-transition-time tr) => 100)
(check (tzif-transition-time (tzif-transition 0 0 #f "UTC" #f #f)) => 0)

(check-report)
