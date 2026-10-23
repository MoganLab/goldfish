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

;; 4. Emacs/Readline 常用编辑快捷键集成测试
(let ((ti (make-text-input "hello world")))
  ;; ctrl-a / ctrl-e: 行首/行尾
  (component-handle-input ti 'ctrl-a)
  (check (editor-cursor ti) => '(0 0))
  (component-handle-input ti 'ctrl-e)
  (check (editor-cursor ti) => '(0 11))
  ;; ctrl-b / ctrl-f: 左右单字
  (component-handle-input ti 'ctrl-b)
  (check (editor-cursor ti) => '(0 10))
  (component-handle-input ti 'ctrl-f)
  (check (editor-cursor ti) => '(0 11))
  ;; ctrl-left / ctrl-right: 单词移动
  (component-handle-input ti 'ctrl-left)
  (check (editor-cursor ti) => '(0 6))
  (component-handle-input ti 'ctrl-right)
  (check (editor-cursor ti) => '(0 11))
  ;; ctrl-w: 向左删除单词
  (component-handle-input ti 'ctrl-w)
  (check (editor-text ti) => "hello ")
  ;; ctrl-u: 删至行首
  (component-handle-input ti 'ctrl-u)
  (check (editor-text ti) => "")
  ;; ctrl-t: 字符对调
  (component-handle-input ti "ab")
  (component-handle-input ti 'ctrl-t)
  (check (editor-text ti) => "ba"))

(check-report)
