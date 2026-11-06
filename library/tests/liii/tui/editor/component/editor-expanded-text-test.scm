;; editor-expanded-text
;; 获取展开全部大段粘贴标记后的编辑器完整真实文本。
;;
;; 语法
;; ----
;; (editor-expanded-text ed)
;;
;; 参数
;; ----
;; ed : editor?
;; 编辑器组件。
;;
;; 返回值
;; ----
;; string?
;; 展开恢复后的完整文本内容。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ed (make-editor "hello")))
  (check (editor-expanded-text ed) => "hello")
) ;let

(check-report)
