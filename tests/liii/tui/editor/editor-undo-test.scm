(import (scheme base)
  (liii check)
  (liii string)
  (liii tui editor)
  (liii tui component)
) ;import

(check-set-mode! 'report)

;; Ctrl+Z 撤销（对齐上游 pi-mono tui.editor.undo）：
;; 快照 = 编辑前 (lines row col pastes)；编辑类操作在变更前记录快照，
;; 连续打字按 fish 式合并为一个撤销单元；提交后清空撤销栈。

;; 辅助：模拟连续打字
(define (type! ed . keys)
  (for-each (lambda (k) (component-handle-input ed k)) keys)
) ;define

;; 1. 撤销栈为空时 Ctrl+Z 不改变内容（键仍被消费）
(let ((ed (make-editor "hello" :multiline #f)))
  (check (component-handle-input ed 'ctrl-z) => #t)
  (check (editor-text ed) => "hello")
) ;let

;; 2. 连续打字合并为一个撤销单元：一次 Ctrl+Z 撤销整词
(let ((ed (make-editor "" :multiline #f)))
  (type! ed "h" "e" "l" "l" "o")
  (check (editor-text ed) => "hello")
  (check (component-handle-input ed 'ctrl-z) => #t)
  (check (editor-text ed) => "")
  (check (editor-cursor ed) => '(0 0))
) ;let

;; 3. 空格自成撤销单元：undo 先撤后词，再撤空格与前词
(let ((ed (make-editor "" :multiline #f)))
  (type! ed "h" "i" " " "t" "h" "e" "r" "e")
  (check (editor-text ed) => "hi there")
  ;; 第一次撤销：回到空格输入前的 "hi"，光标在第 2 列
  (component-handle-input ed 'ctrl-z)
  (check (editor-text ed) => "hi")
  (check (editor-cursor ed) => '(0 2))
  ;; 第二次撤销：回到输入 "h" 之前的空状态
  (component-handle-input ed 'ctrl-z)
  (check (editor-text ed) => "")
  (check (editor-cursor ed) => '(0 0))
) ;let

;; 4. 退格可撤销，光标一并恢复
(let ((ed (make-editor "" :multiline #f)))
  (type! ed "a" "b")
  (component-handle-input ed 'backspace)
  (check (editor-text ed) => "a")
  (component-handle-input ed 'ctrl-z)
  (check (editor-text ed) => "ab")
  (check (editor-cursor ed) => '(0 2))
) ;let

;; 5. 杀词（Ctrl+W）可撤销
(let ((ed (make-editor "" :multiline #f)))
  (type! ed "h" "e" "l" "l" "o")
  (component-handle-input ed 'ctrl-w)
  (check (editor-text ed) => "")
  (component-handle-input ed 'ctrl-z)
  (check (editor-text ed) => "hello")
) ;let

;; 6. 多行换行（Ctrl+J）可撤销，跨行光标恢复
(let ((ed (make-editor "" :multiline #t :submit-on-enter? #t)))
  (type! ed "a" "b")
  (component-handle-input ed 'ctrl-j)
  (type! ed "c" "d")
  (check (editor-text ed) => "ab\ncd")
  (component-handle-input ed 'ctrl-z)
  (check (editor-text ed) => "ab\n")
  (check (editor-cursor ed) => '(1 0))
  (component-handle-input ed 'ctrl-z)
  (check (editor-text ed) => "ab")
) ;let

;; 7. editor-set-text! 清空可撤销：Ctrl+Z 找回被清掉的草稿
(let ((ed (make-editor "" :multiline #f)))
  (type! ed "a" "b" "c")
  (editor-set-text! ed "")
  (check (editor-text ed) => "")
  (component-handle-input ed 'ctrl-z)
  (check (editor-text ed) => "abc")
  (check (editor-cursor ed) => '(0 3))
  (component-handle-input ed 'ctrl-z)
  (check (editor-text ed) => "")
) ;let

;; 8. 提交后撤销栈清空：提交动作本身不可撤销
(let* ((submitted #f)
       (ed (make-editor "" :multiline #f
             :on-submit (lambda (comp text) (set! submitted text))
          ) ;make-editor
       ) ;ed
   ) ;
  (type! ed "h" "i")
  (component-handle-input ed 'enter)
  (check submitted => "hi")
  ;; 提交清栈：Ctrl+Z 无内容可撤
  (component-handle-input ed 'ctrl-z)
  (check (editor-text ed) => "hi")
) ;let

;; 9. 大段粘贴（折叠为标记）撤销：文本与粘贴注册表一并恢复
(let* ((lines '(1 2 3 4 5 6 7 8 9 10 11 12))
       (content (apply string-append
                  (map (lambda (i) (string-append "line-" (number->string i) "\n")) lines)
                ) ;map
             ) ;apply
       (paste-key (string-append "\x1b;[200~" content "\x1b;[201~"))
       (ed (make-editor "" :multiline #t :submit-on-enter? #t))
      ) ;
  (type! ed "x")
  (component-handle-input ed paste-key)
  (check-true (string-contains? (editor-text ed) "[paste #1"))
  (check-true (string-contains? (editor-expanded-text ed) "line-12"))
  ;; 撤销粘贴：标记消失，注册表恢复为空
  (component-handle-input ed 'ctrl-z)
  (check (editor-text ed) => "x")
  (check (editor-expanded-text ed) => "x")
  ;; 再撤销 "x"
  (component-handle-input ed 'ctrl-z)
  (check (editor-text ed) => "")
) ;let*

;; 10. 历史浏览（单行 Up 进入）可撤销：一次 Ctrl+Z 回到浏览前草稿
(let ((ed (make-text-input "")))
  (history-push! (editor-history ed) "old1")
  (type! ed "d" "r" "a" "f" "t")
  (component-handle-input ed 'up)
  (check (editor-text ed) => "old1")
  ;; 再次向上停在顶部，不产生新快照
  (component-handle-input ed 'up)
  (check (editor-text ed) => "old1")
  (component-handle-input ed 'ctrl-z)
  (check (editor-text ed) => "draft")
  (check (editor-cursor ed) => '(0 5))
) ;let

(check-report)
