;; tzif-transition-standard?
;; 读取 tzif-transition 记录的 转移类型是否为本地标准时间 字段。
;;
;; 语法
;; ----
;; (tzif-transition-standard? tr)
;;
;; 参数
;; ----
;; tr : tzif-transition? 转移记录。
;;
;; 返回值
;; ----
;; boolean? 转移类型是否为本地标准时间。

(import (liii check) (liii date-time internal tzif))

(check-set-mode! 'report-failed)

(check (tzif-transition-standard? (tzif-transition 100 3600 #t "EDT" #f #f))
  =>
  #f
) ;check
(check (tzif-transition-standard? (tzif-transition 0 0 #f "UTC" #t #f)) => #t)

(check-report)
