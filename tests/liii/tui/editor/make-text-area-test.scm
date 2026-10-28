(import (liii check)
        (liii tui component)
        (liii tui editor component))

(check-set-mode! 'report-failed)

;; make-text-area & text-area? 测试

;; 1. make-text-area 构造与类型谓词
(let ((ta (make-text-area "line1\nline2")))
  (check (component? ta) => #t)
  (check (editor? ta) => #t)
  (check (text-area? ta) => #t)
  (check (editor-text ta) => "line1\nline2")
  (check (editor-lines ta) => '("line1" "line2")))

;; 2. 多行模式下 Enter 由 submit-on-enter? 控制，不再插入换行
(let ((ta (make-text-area "hello")))
  (check (component-handle-input ta 'enter) => #t)
  (check (editor-lines ta) => '("hello")))

;; 3. 多行模式下上下键移动光标
(let ((ta (make-text-area "line 1\nline 2")))
  (editor-set-cursor! ta 0 2)
  (check (component-handle-input ta 'down) => #t)
  (check (editor-cursor ta) => '(1 2))
  (check (component-handle-input ta 'up) => #t)
  (check (editor-cursor ta) => '(0 2)))

(check-report)
