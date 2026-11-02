;; pi-footer-set-thinking-level!
;; 更新状态栏上显示的思维链等级。
;;
;; 语法
;; ----
;; (pi-footer-set-thinking-level! comp tl)
;;
;; 参数
;; ----
;; comp : component?
;; 底部状态栏组件实例。
;; tl : string?
;; 新的思考等级（如 "off"、"low"、"medium"、"high"）。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive footer)
) ;import

(check-set-mode! 'report-failed)

;; 1. 设置 thinking level 并验证渲染体现
(let* ((ft (make-pi-footer :model "claude-3-5-sonnet" :supports-thinking #t))
       (lines1 (component-render ft 100))
      ) ;
  (check-false (string-contains? (string-join lines1 "\n") "high"))
  (pi-footer-set-thinking-level! ft "high")
  (let ((lines2 (component-render ft 100)))
    (check-true (string-contains? (string-join lines2 "\n") "high"))
  ) ;let
) ;let*

(check-report)
