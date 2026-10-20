(import (liii check)
        (liii tui component)
        (liii tui editor component))

(check-set-mode! 'report-failed)

;; make-editor 构造与基础属性及事件测试

;; 1. 默认单行编辑器
(let ((ed (make-editor)))
  (check (component? ed) => #t)
  (check (editor? ed) => #t)
  (check (editor-text ed) => "")
  (check (editor-cursor ed) => '(0 0))
  (check (editor-focused? ed) => #t)
  (check (editor-placeholder ed) => "")
  (check (editor-history-list ed) => '()))

;; 2. 初始文本与光标初始化在末尾
(let ((ed (make-editor "init")))
  (check (editor-text ed) => "init")
  (check (editor-cursor ed) => '(0 4)))

;; 3. component-handle-input 输入字符与移动光标
(let ((ed (make-editor)))
  (check (component-handle-input ed "a") => #t)
  (check (component-handle-input ed "b") => #t)
  (check (editor-text ed) => "ab")
  (check (editor-cursor ed) => '(0 2))

  ;; 左移
  (check (component-handle-input ed 'left) => #t)
  (check (editor-cursor ed) => '(0 1))

  ;; 插入 'X'
  (check (component-handle-input ed "X") => #t)
  (check (editor-text ed) => "aXb")
  (check (editor-cursor ed) => '(0 2))

  ;; Home & End
  (check (component-handle-input ed 'home) => #t)
  (check (editor-cursor ed) => '(0 0))
  (check (component-handle-input ed 'end) => #t)
  (check (editor-cursor ed) => '(0 3))

  ;; 退格
  (check (component-handle-input ed 'backspace) => #t)
  (check (editor-text ed) => "aX")
  (check (editor-cursor ed) => '(0 2)))

;; 4. component-render 渲染
(let ((ed (make-editor "hi" :cursor-style 'bar)))
  (editor-set-cursor! ed 0 1)
  (check (component-render ed 20) => '("h|i")))

;; 5. on-change 回调
(let* ((changed-val #f)
       (ed (make-editor "" :on-change (lambda (c val) (set! changed-val val)))))
  (component-handle-input ed "ok")
  (check changed-val => "ok"))

(check-report)
