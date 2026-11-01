(import (liii check) (liii tui editor component) (liii tui editor handle))

(check-set-mode! 'report-failed)

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
;; key : symbol? / string? / char?
;; 按键。符号表示控制键（如 'left、'enter、'ctrl-z）；
;; 字符串表示普通文本输入，或带括号标记的粘贴文本；
;; 字符表示单个字符输入。
;;
;; 返回值
;; ----
;; boolean
;; #t 表示按键已被编辑器消费；#f 表示按键未被处理，应交由上层组件处理。
;;
;; 说明
;; ----
;; 该函数是编辑器组件的输入回调，由组件框架收到按键时调用，
;; 也可以直接调用它来检查按键处理行为。
;; 支持的控制键包括：左右移动、home/end、按词移动、上下键（历史回溯）、
;; backspace/delete、ctrl-k/ctrl-u/ctrl-w/ctrl-t、shift-enter/ctrl-enter/ctrl-j（换行）、ctrl-z 撤销与 enter 提交。
;; 字符串按键若为括号粘贴标记，则按粘贴处理，大段内容折叠为 [paste #N ...] 占位。
;;
;; 错误处理
;; ----
;; type-error 当 comp 不是组件时。

;; 1. 普通字符输入
(let ((ed (make-editor)))
  (check (editor-handle-input ed "a") => #t)
  (check (editor-handle-input ed "b") => #t)
  (check (editor-text ed) => "ab")
  (check (editor-cursor ed) => '(0 2))
) ;let

;; 2. 移动按键
(let ((ed (make-editor "abc")))
  (check (editor-handle-input ed 'left) => #t)
  (check (editor-cursor ed) => '(0 2))
  (check (editor-handle-input ed 'home) => #t)
  (check (editor-cursor ed) => '(0 0))
  (check (editor-handle-input ed 'end) => #t)
  (check (editor-cursor ed) => '(0 3))
) ;let

;; 3. 退格与删除
(let ((ed (make-editor "abc")))
  (check (editor-handle-input ed 'backspace) => #t)
  (check (editor-text ed) => "ab")
  ;; 光标位于行尾时没有可删除的内容
  (check (editor-handle-input ed 'delete) => #f)
  (check (editor-text ed) => "ab")
  ;; 移到行首后 delete 删除光标处的字符
  (editor-handle-input ed 'home)
  (check (editor-handle-input ed 'delete) => #t)
  (check (editor-text ed) => "b")
) ;let

;; 4. 未处理的按键返回 #f
(let ((ed (make-editor)))
  (check (editor-handle-input ed 'f5) => #f)
  (check (editor-handle-input ed 'ctrl-x) => #f)
) ;let

;; 5. ctrl-z 撤销输入
(let ((ed (make-editor)))
  (editor-handle-input ed "x")
  (check (editor-text ed) => "x")
  (check (editor-handle-input ed 'ctrl-z) => #t)
  (check (editor-text ed) => "")
) ;let

;; 6. enter 触发提交回调，shift-enter 触发多行换行
(let* ((submitted #f)
       (ed (make-editor "hi" :multiline #t :submit-on-enter? #t
             :on-submit (lambda (c text) (set! submitted text)))))
  (check (editor-handle-input ed 'shift-enter) => #t)
  (check (editor-text ed) => "hi\n")
  (check submitted => #f)
  (check (editor-handle-input ed 'enter) => #t)
  (check submitted => "hi\n")
) ;let

;; 7. 括号粘贴标记按粘贴处理
(let ((ed (make-editor)))
  (check (editor-handle-input ed "\x1b;[200~abc\x1b;[201~") => #t)
  (check (editor-text ed) => "abc")
) ;let

(check-report)
