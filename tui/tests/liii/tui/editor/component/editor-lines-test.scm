;; editor-lines
;; 获取编辑器的文本行列表。
;;
;; 语法
;; ----
;; (editor-lines ed)
;;
;; 参数
;; ----
;; ed : editor?
;; 编辑器组件。
;;
;; 返回值
;; ----
;; list?
;; 字符串行列表。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ed (make-text-area "a\nb")))
  (check (editor-lines ed) => '("a" "b"))
) ;let

(check-report)
