;; pi-config-aio-modal-thinking-picker
;; 构造调整已有槽位思考强度的选择覆盖层状态。
;;
;; 语法
;; ----
;; (pi-config-aio-modal-thinking-picker chain-id node-index provider-id model-id cur-level)
;;
;; 参数
;; ----
;; chain-id : string
;; 链条 id。
;; node-index : integer
;; 0-based 节点下标。
;; provider-id : string
;; 供应商 id。
;; model-id : string
;; 模型 id。
;; cur-level : string
;; 当前思考等级。
;;
;; 返回值
;; ------
;; list
;; 覆盖层状态 alist。

(import (scheme base)
  (liii check)
  (liii pi code modes config aio-modal)
) ;import

(check-set-mode! 'report-failed)

(let ((m (pi-config-aio-modal-thinking-picker "coding-fast" 0 "google" "gemini-3.8-flash" "high")))
  (check (cdr (assoc 'mode m)) => 'pick-slot-thinking)
  (check (cdr (assoc 'chain-id m)) => "coding-fast")
  (check (cdr (assoc 't-idx m)) => 4)
) ;let

(check-report)