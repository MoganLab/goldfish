;; tool-card-set-complete!
;; 标记工具卡片执行完成并记录状态、耗时与结果。
;;
;; 语法
;; ----
;; (tool-card-set-complete! card result is-error? duration-ms)
;;
;; 参数
;; ----
;; card : component?
;; 工具卡片组件。
;; result : any
;; 工具最终返回结果。
;; is-error? : boolean?
;; 是否以错误结束。
;; duration-ms : integer?
;; 执行耗时（毫秒）。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive views)
) ;import

(check-set-mode! 'report-failed)

;; 1. 标记执行成功
(let* ((card (make-tool-card "bash" '((command . "ls")) 'running))
       (_ (tool-card-set-complete! card "file1\nfile2" #f 150))
       (lines (component-render card 80))
      ) ;
  (check-true (pair? lines))
) ;let*

;; 2. 标记执行失败
(let* ((card (make-tool-card "bash" '((command . "cat no")) 'running))
       (_ (tool-card-set-complete! card "not found" #t 20))
       (lines (component-render card 80))
      ) ;
  (check-true (pair? lines))
) ;let*

(check-report)
