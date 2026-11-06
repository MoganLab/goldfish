;; editor-history
;; 获取编辑器内部关联的历史管理器实例。
;;
;; 语法
;; ----
;; (editor-history ed)
;;
;; 参数
;; ----
;; ed : editor?
;; 编辑器组件。
;;
;; 返回值
;; ----
;; history?
;; 历史管理器实例。

(import (scheme base)
  (liii check)
  (liii tui editor history)
  (liii tui editor component)
) ;import

(check-set-mode! 'report-failed)

(let ((ed (make-editor "hello")))
  (check-true (history? (editor-history ed)))
) ;let

(check-report)
