;; tool-card-set-output!
;; 原地更新工具卡片的输出内容。
;;
;; 语法
;; ----
;; (tool-card-set-output! card result)
;;
;; 参数
;; ----
;; card : component?
;; 工具卡片组件。
;; result : any
;; 工具部分或最终执行结果。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive views)
) ;import

(check-set-mode! 'report-failed)

;; 1. 更新卡片输出
(let* ((card (make-tool-card "bash" '((command . "echo 1")) 'running))
       (_ (tool-card-set-output! card "output line 1"))
       (lines (component-render card 80))
      ) ;
  (check-true (pair? lines))
) ;let*

(check-report)
