;; pi-footer-pending-model
;; 获取请求进行中记录的原模型快照（(provider model tl supports? active-node) 或 #f）。
;;
;; 语法
;; ----
;; (pi-footer-pending-model comp)
;;
;; 参数
;; ----
;; comp : pi-footer?
;; 页脚组件。
;;
;; 返回值
;; ----
;; (or list #f)
;; 无待切换原模型时返回 #f；否则返回 5 元组快照。

(import (scheme base)
  (liii check)
  (liii pi code modes interactive footer)
) ;import

(check-set-mode! 'report)

(let ((ft (make-pi-footer :provider "minimax-cn" :model "MiniMax-M3.1-Flash")))
  (check (pi-footer-pending-model ft) => #f)
  (pi-footer-set-pending-model! ft "minimax-cn" "MiniMax-M3.1-Flash" "medium" #t)
  (check (pi-footer-pending-model ft)
    =>
    '("minimax-cn" "MiniMax-M3.1-Flash" "medium" #t #f)
  ) ;check
  (pi-footer-clear-pending-model! ft)
  (check (pi-footer-pending-model ft) => #f)
) ;let

(check-report)
