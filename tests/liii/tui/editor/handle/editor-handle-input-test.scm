;; editor-handle-input
;; 处理编辑器的按键输入，返回该按键是否已被编辑器消费。
;;
;; 语法
;; ----
;; (editor-handle-input comp key)
;;
;; 参数
;; ----
;; comp : component?
;; 编辑器组件，由 make-editor、make-text-input 或 make-text-area 构造。
;;
;; key : any
;; 按键符号、字符串或字符。
;;
;; 返回值
;; ----
;; boolean?
;; #t 表示按键已被编辑器消费；#f 表示未被处理。

(import (scheme base)
  (liii check)
  (liii tui editor component)
  (liii tui editor handle)
) ;import

(check-set-mode! 'report-failed)

;; 1. 普通文本输入
(let ((ed (make-text-input)))
  (check-true (editor-handle-input ed "h"))
  (check-true (editor-handle-input ed "i"))
  (check (editor-text ed) => "hi")
) ;let

;; 2. 控制键
(let ((ed (make-text-input "abc")))
  (check-true (editor-handle-input ed 'left))
  (check (editor-cursor ed) => '(0 2))
  (check-true (editor-handle-input ed 'home))
  (check (editor-cursor ed) => '(0 0))
) ;let

;; 3. 未被消费的按键返回 #f
(let ((ed (make-text-input)))
  (check-false (editor-handle-input ed 'f1))
) ;let

;; 4. 类型检查
(check-catch 'type-error (editor-handle-input "not-an-editor" 'left))

(check-report)
