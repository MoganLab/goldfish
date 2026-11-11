;; make-all-in-one-cfg
;; 构造 all-in-one 全局配置记录。
;;
;; 语法
;; ----
;; (make-all-in-one-cfg continuous? max-wait-ms retry-interval-ms probe-timeout-ms chains)
;;
;; 参数
;; ----
;; continuous? : boolean
;; 全链等待连续运行开关。
;; max-wait-ms : number
;; 单次最大等待毫秒数。
;; retry-interval-ms : number
;; 重试轮询间隔毫秒数。
;; probe-timeout-ms : number
;; 单节点金丝雀预检探测超时毫秒数。
;; chains : list of <chain-def>
;; 链定义列表。
;;
;; 返回值
;; ----
;; <all-in-one-cfg>
;; 配置记录对象。

(import (scheme base) (liii check) (liii pi all_in_one types))

(check-set-mode! 'report-failed)

(let ((cfg (make-all-in-one-cfg #t 3600000 60000 8000 '())))
  (check-true (all-in-one-cfg? cfg))
  (check (all-in-one-cfg-continuous? cfg) => #t)
  (check (all-in-one-cfg-max-wait-ms cfg) => 3600000)
  (check (all-in-one-cfg-retry-interval-ms cfg) => 60000)
  (check (all-in-one-cfg-probe-timeout-ms cfg) => 8000)
  (check (all-in-one-cfg-chains cfg) => '())
) ;let

(check-report)
