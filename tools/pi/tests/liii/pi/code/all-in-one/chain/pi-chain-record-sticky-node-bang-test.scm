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

(import (scheme base)
  (liii check)
  (liii pi code all-in-one chain)
) ;import

(check-set-mode! 'report-failed)

(pi-chain-reset-sticky-state!)
(check (pi-chain-current-sticky-index "test-chain") => #f)

(pi-chain-record-sticky-node! "test-chain" 1)
(check (pi-chain-current-sticky-index "test-chain") => 1)

(pi-chain-record-sticky-node! "test-chain" 2)
(check (pi-chain-current-sticky-index "test-chain") => 2)

(pi-chain-reset-sticky-state!)
(check (pi-chain-current-sticky-index "test-chain") => #f)

(check-report)
