;; editor-set-border-color!
;; 设置编辑器的边框 ANSI 颜色序列。
;;
;; 语法
;; ----
;; (editor-set-border-color! ed color)
;;
;; 参数
;; ----
;; ed : editor?
;; 编辑器组件。
;;
;; color : string? 或 #f
;; 颜色序列或 #f。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ed (make-editor)))
  (editor-set-border-color! ed "\x1b;[31m")
  (check (editor-border-color ed) => "\x1b;[31m")
) ;let

(check-report)
