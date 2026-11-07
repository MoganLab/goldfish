;; tool-card-expanded?
;; 查询工具卡片当前是否处于展开状态。
;;
;; 语法
;; ----
;; (tool-card-expanded? card)
;;
;; 参数
;; ----
;; card : component?
;; 工具卡片组件。
;;
;; 返回值
;; ----
;; boolean?
;; 处于展开状态返回 #t，折叠状态返回 #f。

(import (scheme base)
  (liii check)
  (liii pi code modes interactive views tool-card)
) ;import

(check-set-mode! 'report-failed)

;; 1. 默认折叠状态为 #f
(let ((card (make-tool-card "read" '((path . "file.txt")) 'success "content")))
  (check-false (tool-card-expanded? card))
  ;; 2. 设置展开后返回 #t
  (tool-card-set-expanded! card #t)
  (check-true (tool-card-expanded? card))
) ;let

(check-report)
