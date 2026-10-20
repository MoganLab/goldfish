(import (liii check)
        (liii tui component)
        (liii tui editor component))

(check-set-mode! 'report-failed)

;; make-text-input & text-input? 测试

;; 1. make-text-input 构造与类型谓词
(let ((ti (make-text-input "hello" :placeholder "enter text...")))
  (check (component? ti) => #t)
  (check (editor? ti) => #t)
  (check (text-input? ti) => #t)
  (check (editor-text ti) => "hello")
  (check (editor-placeholder ti) => "enter text..."))

;; 2. 回车提交与历史记录自动记录
(let* ((submitted #f)
       (ti (make-text-input "test command"
             :on-submit (lambda (c text) (set! submitted text)))))
  (check (component-handle-input ti 'enter) => #t)
  (check submitted => "test command")
  (check (editor-history-list ti) => '("test command")))

;; 3. 历史记录上下导航集成
(let ((ti (make-text-input)))
  (component-handle-input ti "first")
  (component-handle-input ti 'enter)
  (editor-set-text! ti "")

  (component-handle-input ti "second")
  (component-handle-input ti 'enter)
  (editor-set-text! ti "")

  ;; 输入正在打的草稿
  (component-handle-input ti "current draft")
  ;; 向上翻
  (check (component-handle-input ti 'up) => #t)
  (check (editor-text ti) => "second")

  (check (component-handle-input ti 'up) => #t)
  (check (editor-text ti) => "first")

  ;; 向下翻回草稿
  (check (component-handle-input ti 'down) => #t)
  (check (editor-text ti) => "second")

  (check (component-handle-input ti 'down) => #t)
  (check (editor-text ti) => "current draft"))

(check-report)
