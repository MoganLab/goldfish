;; tzif-transition?
;; 判断对象是否为 TZif 转移记录。
;;
;; 语法
;; ----
;; (tzif-transition? obj)
;;
;; 参数
;; ----
;; obj : any? 任意对象。
;;
;; 返回值
;; ----
;; boolean? 是转移记录返回 #t。

(import (liii check) (liii date-time internal tzif))

(check-set-mode! 'report-failed)

(check-true (tzif-transition? (tzif-transition 100 3600 #t "EDT" #f #f)))
(check-false (tzif-transition? "tr"))
(check-false (tzif-transition? 100))

(check-report)
