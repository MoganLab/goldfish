;; all-in-one-cfg-probe-timeout-ms
;; 获取单节点金丝雀预检探测超时（毫秒）。
;;
;; 语法
;; ----
;; (all-in-one-cfg-probe-timeout-ms cfg)
;;
;; 参数
;; ----
;; cfg : <all-in-one-cfg>
;; 配置记录。
;;
;; 返回值
;; ----
;; number
;; 单节点探测超时毫秒数。

(import (scheme base) (liii check) (liii pi all_in_one types))

(check-set-mode! 'report-failed)

(let ((cfg (make-all-in-one-cfg #t 3600000 60000 5000 '())))
  (check (all-in-one-cfg-probe-timeout-ms cfg) => 5000)
) ;let

(check-report)
