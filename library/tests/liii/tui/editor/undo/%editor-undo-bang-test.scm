;; %editor-undo!
;; 执行单步撤销操作，将编辑器恢复到上一个快照状态。
;;
;; 语法
;; ----
;; (%editor-undo! comp data)
;;
;; 参数
;; ----
;; comp : component?
;; 编辑器组件。
;;
;; data : any
;; 编辑器内部数据。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui editor component)
  (liii tui editor handle)
  (liii tui editor undo)
) ;import

(check-set-mode! 'report-failed)

(let* ((ed (make-text-input "")) (data (component-data ed)))
  (editor-handle-input ed "a")
  (check (editor-text ed) => "a")
  (%editor-undo! ed data)
  (check (editor-text ed) => "")
) ;let*

(check-report)
