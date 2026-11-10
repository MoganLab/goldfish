;; editor-set-on-submit!
;; 设置编辑器的回车提交回调过程。
;;
;; 语法
;; ----
;; (editor-set-on-submit! ed on-submit)
;;
;; 参数
;; ----
;; ed : editor?
;; 编辑器组件。
;;
;; on-submit : procedure?
;; 接收 (comp text) 的提交回调过程。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui editor component)
) ;import

(check-set-mode! 'report-failed)

(let* ((submitted #f) (ed (make-text-input "run")))
  (editor-set-on-submit! ed (lambda (c t) (set! submitted t)))
  (component-handle-input ed 'enter)
  (check submitted => "run")
) ;let*

(check-report)
