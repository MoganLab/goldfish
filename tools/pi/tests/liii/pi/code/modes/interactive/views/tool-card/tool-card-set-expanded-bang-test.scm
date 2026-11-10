;; tool-card-set-expanded!
;; 设置工具卡片的展开/折叠显示状态。
;;
;; 语法
;; ----
;; (tool-card-set-expanded! card flag)
;;
;; 参数
;; ----
;; card : component?
;; 工具卡片组件。
;; flag : boolean?
;; 是否展开。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii pi code modes interactive views tool-card)
) ;import

(check-set-mode! 'report-failed)

;; 1. 切换展开与折叠状态
(let ((card (make-tool-card "bash" '((command . "ls")) 'running)))
  (tool-card-set-expanded! card #t)
  (check-true (tool-card-expanded? card))
  (tool-card-set-expanded! card #f)
  (check-false (tool-card-expanded? card))
) ;let

(check-report)
