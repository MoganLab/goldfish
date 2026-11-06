;; tzif-transition-designation
;; 读取 tzif-transition 记录的 转移后的时区缩写 字段。
;;
;; 语法
;; ----
;; (tzif-transition-designation tr)
;;
;; 参数
;; ----
;; tr : tzif-transition? 转移记录。
;;
;; 返回值
;; ----
;; string? 转移后的时区缩写。

(import (liii check) (liii date-time internal tzif))

(check-set-mode! 'report-failed)

(check (tzif-transition-designation (tzif-transition 100 3600 #t "EDT" #f #f))
  =>
  "EDT"
) ;check

(check-report)
