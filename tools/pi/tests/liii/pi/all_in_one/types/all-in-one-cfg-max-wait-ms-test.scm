;; all-in-one-cfg-max-wait-ms
;; 获取全链等待单次最大耗时（毫秒）。
;;
;; 语法
;; ----
;; (all-in-one-cfg-max-wait-ms cfg)
;;
;; 参数
;; ----
;; cfg : <all-in-one-cfg>
;; 配置记录。
;;
;; 返回值
;; ----
;; number
;; 单次最大等待毫秒数。

(import (scheme base) (liii check) (liii pi all_in_one types))

(check-set-mode! 'report-failed)

(let ((cfg (make-all-in-one-cfg #t 1800000 60000 8000 '())))
  (check (all-in-one-cfg-max-wait-ms cfg) => 1800000)
) ;let

(check-report)
