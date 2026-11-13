;; tool-card-pending-bash?
;; 判定组件是否为处于 pending 状态的 bash 工具卡片。
;;
;; 语法
;; ----
;; (tool-card-pending-bash? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检验的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 若为处于 pending 状态的 bash 工具卡片返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii pi code modes interactive views tool-card)
) ;import

(check-set-mode! 'report-failed)

;; 1. pending 状态的 bash 卡片返回 #t
(let ((card (make-tool-card "bash" '((command . "ls")) 'pending)))
  (check-true (tool-card-pending-bash? card))
) ;let

;; 2. 非 pending 的 bash 卡片返回 #f
(let ((card (make-tool-card "bash" '((command . "ls")) 'success "ok")))
  (check-false (tool-card-pending-bash? card))
) ;let

;; 3. 非 bash 的 pending 卡片返回 #f
(let ((card (make-tool-card "read" "file.scm" 'pending)))
  (check-false (tool-card-pending-bash? card))
) ;let

;; 4. 非工具组件或非组件返回 #f
(check-false
  (tool-card-pending-bash? (make-component :render (lambda (s w) '())))
) ;check-false
(check-false (tool-card-pending-bash? #f))
(check-false (tool-card-pending-bash? "bash"))

(check-report)
