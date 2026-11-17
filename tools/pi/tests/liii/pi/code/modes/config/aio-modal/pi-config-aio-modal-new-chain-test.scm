;; pi-config-aio-modal-new-chain
;; 构造新建 all-in-one 伪模型（chain）的输入覆盖层状态。
;;
;; 语法
;; ----
;; (pi-config-aio-modal-new-chain)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; list
;; 覆盖层状态 alist。

(import (scheme base) (liii check) (liii pi code modes config aio-modal))

(check-set-mode! 'report-failed)

(let ((m (pi-config-aio-modal-new-chain)))
  (check (cdr (assoc 'mode m)) => 'new-chain)
  (check (cdr (assoc 'input m)) => "")
  (check (cdr (assoc 'error m)) => #f)
) ;let

(check-report)
