;; tool-card-started-at
;; 获取工具卡片的起始计时时间戳。
;;
;; 语法
;; ----
;; (tool-card-started-at card)
;;
;; 参数
;; ----
;; card : tool-card-component?
;; 工具卡片组件。
;;
;; 返回值
;; ----
;; number? 或 #f
;; pending 状态下记录的起始时间戳（毫秒），非 pending 构造时为 #f。

(import (scheme base)
  (liii check)
  (liii pi code modes interactive views tool-card)
) ;import

(check-set-mode! 'report-failed)

;; 1. pending 卡片记录起始时间戳
(let ((card (make-tool-card "bash" '((command . "ls")) 'pending)))
  (check-true (number? (tool-card-started-at card)))
) ;let

;; 2. 直接以已完成状态构造时为 #f
(let ((card (make-tool-card "bash" '((command . "ls")) 'success "ok")))
  (check-false (tool-card-started-at card))
) ;let

(check-report)
