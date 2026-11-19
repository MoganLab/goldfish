;; pi-chain-record-sticky-node!
;; 记录指定链的粘性节点下标。
;;
;; 语法
;; ----
;; (pi-chain-record-sticky-node! chain-id node-idx)
;;
;; 参数
;; ----
;; chain-id : string
;; 链标识。
;; node-idx : integer
;; 节点下标。
;;
;; 返回值
;; ----
;; unspecified
;; 同一链重复记录会覆盖旧下标。chain-id 非字符串或 node-idx 非非负整数时抛 type-error。

(import (scheme base) (liii check) (liii pi code all-in-one state))

(check-set-mode! 'report-failed)

(pi-chain-reset-state!)
(check (pi-chain-current-sticky-index "test-chain") => #f)

(pi-chain-record-sticky-node! "test-chain" 1)
(check (pi-chain-current-sticky-index "test-chain") => 1)

(pi-chain-record-sticky-node! "test-chain" 2)
(check (pi-chain-current-sticky-index "test-chain") => 2)

(pi-chain-reset-state!)
(check (pi-chain-current-sticky-index "test-chain") => #f)

;; 入参类型错误明确暴露
(check-catch 'type-error (pi-chain-record-sticky-node! #f 0))
(check-catch 'type-error (pi-chain-record-sticky-node! "test-chain" -1))
(check-catch 'type-error (pi-chain-record-sticky-node! "test-chain" "0"))
(check-false (pi-chain-current-sticky-index "test-chain"))

(check-report)
