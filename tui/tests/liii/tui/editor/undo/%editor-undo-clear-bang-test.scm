;; %editor-undo-clear!
;; 清空撤销历史栈。
;;
;; 语法
;; ----
;; (%editor-undo-clear! data)
;;
;; 参数
;; ----
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
  (liii tui editor undo)
) ;import

(check-set-mode! 'report-failed)

(let* ((ed (make-text-input "")) (data (component-data ed)))
  (%editor-undo-clear! data)
  (check-true #t)
) ;let*

(check-report)
