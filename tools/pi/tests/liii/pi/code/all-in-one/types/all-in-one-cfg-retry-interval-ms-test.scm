;; all-in-one-cfg-retry-interval-ms
;; 获取全链等待重试轮询间隔（毫秒）。
;;
;; 语法
;; ----
;; (all-in-one-cfg-retry-interval-ms cfg)
;;
;; 参数
;; ----
;; cfg : <all-in-one-cfg>
;; 配置记录。
;;
;; 返回值
;; ----
;; number
;; 重试轮询间隔毫秒数。

(import (scheme base) (liii check) (liii pi code all-in-one types))

(check-set-mode! 'report-failed)

(let ((cfg (make-all-in-one-cfg #t 3600000 30000 8000 '())))
  (check (all-in-one-cfg-retry-interval-ms cfg) => 30000)
) ;let

(check-report)
