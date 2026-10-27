(import (scheme base)
  (liii check)
  (liii unicode)
  (liii tui component)
  (liii tui editor)
) ;import

(check-set-mode! 'report-failed)

(define (paste-event text)
  (string-append "\x1b;[200~" text "\x1b;[201~")
) ;define

;; 1. 小段多行粘贴直接内联插入，不触发提交
(let ((ed (make-editor "" :multiline #f)))
  (check (component-handle-input ed (paste-event "line1\r\nline2\tend")) => #t)
  (check (editor-text ed) => "line1\nline2    end")
  (check (editor-lines ed) => '("line1" "line2    end"))
  (check (editor-cursor ed) => '(1 12))
  (check (editor-expanded-text ed) => "line1\nline2    end")
) ;let

;; 2. 大段粘贴折叠为 [paste #1 N chars]，提交时展开
(let* ((big (make-string 1965 #\a))
       (submitted #f)
       (ed (make-editor ""
             :multiline #f
             :on-submit (lambda (comp text) (set! submitted text))
           ) ;make-editor
       ) ;ed
      ) ;
  (component-handle-input ed (paste-event big))
  (check (editor-text ed) => "[paste #1 1965 chars]")
  (check (editor-cursor ed) => '(0 21))
  (check (editor-expanded-text ed) => big)
  (check (component-handle-input ed 'enter) => #t)
  (check submitted => big)
) ;let*

;; 3. 多行大粘贴按行数折叠
(let* ((big (string-append "a" (make-string 11 #\newline) "b")) (ed (make-editor "")))
  (component-handle-input ed (paste-event big))
  (check (editor-text ed) => "[paste #1 +12 lines]")
  (check (editor-expanded-text ed) => big)
) ;let*

;; 4. Backspace 整体删除标记并重置编号
(let* ((big (make-string 1965 #\a)) (ed (make-editor "")))
  (component-handle-input ed (paste-event big))
  (check (component-handle-input ed 'backspace) => #t)
  (check (editor-text ed) => "")
  (component-handle-input ed (paste-event big))
  (check (editor-text ed) => "[paste #1 1965 chars]")
) ;let*

;; 5. 删除中间标记后，后面的编号自动前移
(let* ((big (make-string 1965 #\a)) (ed (make-editor "")))
  (component-handle-input ed (paste-event big))
  (component-handle-input ed (paste-event big))
  (component-handle-input ed (paste-event big))
  (check (editor-text ed)
    =>
    "[paste #1 1965 chars][paste #2 1965 chars][paste #3 1965 chars]"
  ) ;check
  (editor-set-cursor! ed 0 21)
  (component-handle-input ed 'backspace)
  (check (editor-text ed) => "[paste #1 1965 chars][paste #2 1965 chars]")
  (check (editor-expanded-text ed) => (string-append big big))
) ;let*

;; 6. Delete 整体删除光标处的标记
(let* ((big (make-string 1965 #\a)) (ed (make-editor "")))
  (component-handle-input ed (paste-event big))
  (editor-set-cursor! ed 0 0)
  (check (component-handle-input ed 'delete) => #t)
  (check (editor-text ed) => "")
) ;let*

;; 7. editor-set-text! 清空注册表并重置编号
(let* ((big (make-string 1965 #\a)) (ed (make-editor "")))
  (component-handle-input ed (paste-event big))
  (editor-set-text! ed "")
  (component-handle-input ed (paste-event big))
  (check (editor-text ed) => "[paste #1 1965 chars]")
) ;let*

;; 7.1 非 ASCII 文本前后的粘贴与整体删除
(let* ((big (utf8-make-string 1001 #\中)) (ed (make-editor "你好")))
  (component-handle-input ed (paste-event big))
  (check (editor-text ed) => "你好[paste #1 1001 chars]")
  (check (editor-expanded-text ed) => (string-append "你好" big))
  (component-handle-input ed 'backspace)
  (check (editor-text ed) => "你好")
) ;let*

;; 8. 空粘贴不改变内容
(let ((ed (make-editor "x")))
  (check (component-handle-input ed (paste-event "")) => #t)
  (check (editor-text ed) => "x")
) ;let

(check-report)
