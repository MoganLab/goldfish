;; editor-history-list
;; 获取编辑器内部历史管理器的全部条目列表。
;;
;; 语法
;; ----
;; (editor-history-list ed)
;;
;; 参数
;; ----
;; ed : editor?
;; 编辑器组件。
;;
;; 返回值
;; ----
;; list?
;; 历史条目字符串列表。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui editor component)
) ;import

(check-set-mode! 'report-failed)

(let ((ed (make-text-input "cmd1")))
  (component-handle-input ed 'enter)
  (check (editor-history-list ed) => '("cmd1"))
) ;let

(check-report)
