;; editor-set-max-lines!
;; 设置编辑器的最大可视行数配置。
;;
;; 语法
;; ----
;; (editor-set-max-lines! ed max-lines)
;;
;; 参数
;; ----
;; ed : editor?
;; 编辑器组件。
;;
;; max-lines : integer? 或 #f
;; 最大可视行数（非负整数或 #f）。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ed (make-editor)))
  (editor-set-max-lines! ed 10)
  (check (editor-max-lines ed) => 10)
) ;let

(check-report)
