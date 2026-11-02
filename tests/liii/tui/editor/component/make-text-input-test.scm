;; make-text-input
;; 创建单行文本输入框组件。
;;
;; 语法
;; ----
;; (make-text-input [text] [opts ...])
;;
;; 参数
;; ----
;; text : string? (可选)
;; 初始文本内容。
;;
;; opts : keyword arguments
;; 支持 :placeholder, :on-submit, :on-change 等选项。
;;
;; 返回值
;; ----
;; component?
;; 包装为 component 的文本输入框实例。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui editor component)
) ;import

(check-set-mode! 'report-failed)

;; 1. make-text-input 构造与类型谓词
(let ((ti (make-text-input "hello" :placeholder "enter text...")))
  (check-true (component? ti))
  (check-true (editor? ti))
  (check-true (text-input? ti))
  (check (editor-text ti) => "hello")
  (check (editor-placeholder ti) => "enter text...")
) ;let

;; 2. 回车提交与历史记录自动记录
(let* ((submitted #f)
       (ti (make-text-input "test command"
             :on-submit (lambda (c text) (set! submitted text))
           ) ;make-text-input
       ) ;ti
      ) ;
  (check-true (component-handle-input ti 'enter))
  (check submitted => "test command")
  (check (editor-history-list ti) => '("test command"))
) ;let*

(check-report)
