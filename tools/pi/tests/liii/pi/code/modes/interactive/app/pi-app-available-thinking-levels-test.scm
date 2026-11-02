;; pi-app-available-thinking-levels
;; 获取当前应用实例支持的思考级别字符串列表。
;;
;; 语法
;; ----
;; (pi-app-available-thinking-levels app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; list of string
;; 支持的思考级别名称列表。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 验证获取到的思考级别为非空列表
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (levels (pi-app-available-thinking-levels app))
      ) ;
  (check-true (list? levels))
  (check-true (pair? levels))
) ;let*

(check-report)
