;; make-editor
;; 创建通用编辑器组件，支持单行/多行、边框、占位符、历史导航与撤销。
;;
;; 语法
;; ----
;; (make-editor [text] [:multiline multiline?] [:placeholder placeholder]
;;   [:border-top bt] [:border-bottom bb] [:border-color bc] [:max-lines max-lines]
;;   [:cursor-style cs] [:submit-on-enter soe] [:on-submit on-submit] [:on-change on-change])
;;
;; 参数
;; ----
;; text : string? (可选，默认为 "")
;; 初始文本内容。
;;
;; multiline : boolean? (可选，默认为 #f)
;; 是否支持多行编辑。
;;
;; placeholder : string? (可选，默认为 "")
;; 占位提示文本。
;;
;; border-top : boolean? (可选，默认为 #f)
;; 是否显示上边框。
;;
;; border-bottom : boolean? (可选，默认为 #f)
;; 是否显示下边框。
;;
;; border-color : string? (可选，默认为 #f)
;; 边框 ANSI 颜色转义序列。
;;
;; max-lines : integer? (可选，默认为 #f)
;; 最大可视行数。
;;
;; cursor-style : symbol? (可选，默认为 'block)
;; 光标样式。
;;
;; submit-on-enter : boolean? (可选)
;; 是否在按 Enter 时触发提交。
;;
;; on-submit : procedure? (可选)
;; 提交回调过程。
;;
;; on-change : procedure? (可选)
;; 文本或光标变化回调过程。
;;
;; 返回值
;; ----
;; component?
;; 编辑器组件实例。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui editor component)
) ;import

(check-set-mode! 'report-failed)

;; 1. 默认单行编辑器
(let ((ed (make-editor)))
  (check-true (component? ed))
  (check-true (editor? ed))
  (check (editor-text ed) => "")
  (check (editor-cursor ed) => '(0 0))
  (check-true (editor-focused? ed))
  (check (editor-placeholder ed) => "")
  (check (editor-history-list ed) => '())
) ;let

;; 2. 初始文本与光标初始化在末尾
(let ((ed (make-editor "init")))
  (check (editor-text ed) => "init")
  (check (editor-cursor ed) => '(0 4))
) ;let

(check-report)
