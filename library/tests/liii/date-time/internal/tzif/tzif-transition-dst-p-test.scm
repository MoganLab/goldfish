;; tzif-transition-dst?
;; 读取 tzif-transition 记录的 转移后是否处于夏令时 字段。
;;
;; 语法
;; ----
;; (tzif-transition-dst? tr)
;;
;; 参数
;; ----
;; tr : tzif-transition? 转移记录。
;;
;; 返回值
;; ----
;; boolean? 转移后是否处于夏令时。

(import (liii check) (liii date-time internal tzif))

(check-set-mode! 'report-failed)

(check (tzif-transition-dst? (tzif-transition 100 3600 #t "EDT" #f #f)) => #t)
(check (tzif-transition-dst? (tzif-transition 0 0 #f "UTC" #f #f)) => #f)

(check-report)
