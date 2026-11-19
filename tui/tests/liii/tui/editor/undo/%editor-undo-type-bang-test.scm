;; %editor-undo-type!
;; 打字输入时的撤销快照记录（支持连续输入字符合并）。
;;
;; 语法
;; ----
;; (%editor-undo-type! data text)
;;
;; 参数
;; ----
;; data : any
;; 编辑器内部数据记录。
;;
;; text : string?
;; 即将输入的文本。
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
  (%editor-undo-type! data "a")
  (%editor-undo-type! data "b")
  (check-true #t)
) ;let*

(check-report)
