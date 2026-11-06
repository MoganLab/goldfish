;; tzif-transition-universal-time?
;; 读取 tzif-transition 记录的 转移时刻是否按 字段。
;;
;; 语法
;; ----
;; (tzif-transition-universal-time? tr)
;;
;; 参数
;; ----
;; tr : tzif-transition? 转移记录。
;;
;; 返回值
;; ----
;; boolean? 转移时刻是否按 UTC 解释。

(import (liii check) (liii date-time internal tzif))

(check-set-mode! 'report-failed)

(check (tzif-transition-universal-time? (tzif-transition 100 3600 #t "EDT" #f #f))
  =>
  #f
) ;check
(check (tzif-transition-universal-time? (tzif-transition 0 0 #f "UTC" #f #t))
  =>
  #t
) ;check

(check-report)
