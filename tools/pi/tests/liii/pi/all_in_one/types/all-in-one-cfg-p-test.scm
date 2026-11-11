;; all-in-one-cfg?
;; 判定对象是否为 all-in-one 全局配置记录。
;;
;; 语法
;; ----
;; (all-in-one-cfg? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查对象。
;;
;; 返回值
;; ----
;; boolean
;; 为配置记录时返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi all_in_one types))

(check-set-mode! 'report-failed)

(check-true (all-in-one-cfg? (make-all-in-one-cfg #t 1000 100 50 '())))
(check-false (all-in-one-cfg? "not-a-cfg"))
(check-false (all-in-one-cfg? 789))

(check-report)
