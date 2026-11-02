;; make-text-area
;; 创建多行文本区域组件。
;;
;; 语法
;; ----
;; (make-text-area [text] [opts ...])
;;
;; 参数
;; ----
;; text : string? (可选)
;; 初始多行文本内容。
;;
;; opts : keyword arguments
;; 支持 :placeholder, :max-lines, :on-submit 等选项。
;;
;; 返回值
;; ----
;; component?
;; 包装为 component 的多行文本区域实例。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui editor component)
) ;import

(check-set-mode! 'report-failed)

;; 1. make-text-area 构造与类型谓词
(let ((ta (make-text-area "line1\nline2")))
  (check-true (component? ta))
  (check-true (editor? ta))
  (check-true (text-area? ta))
  (check (editor-text ta) => "line1\nline2")
  (check (editor-lines ta) => '("line1" "line2"))
) ;let

(check-report)
