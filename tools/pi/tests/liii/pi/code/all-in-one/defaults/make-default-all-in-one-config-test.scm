;; make-default-all-in-one-config
;; 构造 all-in-one 默认全局配置记录。
;;
;; 语法
;; ----
;; (make-default-all-in-one-config)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; <all-in-one-cfg>
;; continuous=#t、maxWaitMs=3600000、retryIntervalMs=60000、
;; probeTimeoutMs=8000，chains 为 all-in-one-default-chains。
;;
;; 说明
;; ----
;; 这是默认参数的唯一真值源：provider 目录回退、request 回退、
;; 压缩窗口回退与配置 TUI 回退都从这里取默认值，不再各自重复
;; 书写 (make-all-in-one-cfg #t 3600000 60000 8000 ...)。每次调用都返回
;; 新记录，调用方可以放心改写字段而不会污染其他调用。

(import (scheme base)
  (liii check)
  (liii pi code all-in-one defaults)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(let* ((cfg (make-default-all-in-one-config)) (chains (all-in-one-cfg-chains cfg)))
  (check-true (all-in-one-cfg? cfg))
  (check (all-in-one-cfg-continuous? cfg) => #t)
  ;; 1 小时 / 60 秒 / 8 秒的毫秒表示
  (check (all-in-one-cfg-max-wait-ms cfg) => 3600000)
  (check (all-in-one-cfg-retry-interval-ms cfg) => 60000)
  (check (all-in-one-cfg-probe-timeout-ms cfg) => 8000)
  (check (length chains) => 2)
  (check (chain-def-id (car chains)) => "coding-fast")
  (check (chain-def-id (cadr chains)) => "coding-high-quality")
  ;; 默认链与 all-in-one-default-chains 同源（逐项等值，而不是各自维护一份）
  (check (map chain-def-id chains)
    =>
    (map chain-def-id (all-in-one-default-chains))
  ) ;check
) ;let*

;; 每次调用返回新记录，改写一个不影响另一个
(let* ((a (make-default-all-in-one-config)) (b (make-default-all-in-one-config)))
  (check (eq? a b) => #f)
  (check (all-in-one-cfg-max-wait-ms a) => (all-in-one-cfg-max-wait-ms b))
) ;let*

(check-report)
