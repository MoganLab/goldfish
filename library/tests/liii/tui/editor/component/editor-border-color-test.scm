;; editor-border-color
;; 获取编辑器配置的边框 ANSI 颜色序列。
;;
;; 语法
;; ----
;; (editor-border-color ed)
;;
;; 参数
;; ----
;; ed : editor?
;; 编辑器组件。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 边框颜色序列或 #f。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ed (make-editor :border-color "\x1b;[32m")))
  (check (editor-border-color ed) => "\x1b;[32m")
) ;let

(check-report)
