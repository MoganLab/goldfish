;; tzif-transition-offset
;; 读取 tzif-transition 记录的 转移后的 字段。
;;
;; 语法
;; ----
;; (tzif-transition-offset tr)
;;
;; 参数
;; ----
;; tr : tzif-transition? 转移记录。
;;
;; 返回值
;; ----
;; integer? 转移后的 UTC 偏移秒数。

(import (liii check) (liii date-time internal tzif))

(check-set-mode! 'report-failed)

(check (tzif-transition-offset (tzif-transition 100 3600 #t "EDT" #f #f))
  =>
  3600
) ;check

(check-report)
