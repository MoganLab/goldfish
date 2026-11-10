;; editor-max-lines
;; 获取编辑器的最大可视行数配置。
;;
;; 语法
;; ----
;; (editor-max-lines ed)
;;
;; 参数
;; ----
;; ed : editor?
;; 编辑器组件。
;;
;; 返回值
;; ----
;; integer? 或 #f
;; 最大可视行数，未限制时返回 #f。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ed (make-editor :max-lines 5)))
  (check (editor-max-lines ed) => 5)
) ;let

(check-report)
