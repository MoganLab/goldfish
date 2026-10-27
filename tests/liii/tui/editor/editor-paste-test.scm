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

;; 1. 10 行以内展开插入，保留多行内容且不触发提交
(let ((ed (make-editor "" :multiline #f)))
  (check (component-handle-input ed (paste-event "line1\r\nline2\tend")) => #t)
  (check (editor-text ed) => "line1\nline2    end")
  (check (editor-lines ed) => '("line1" "line2    end"))
  (check (editor-cursor ed) => '(1 12))
  (check (editor-expanded-text ed) => "line1\nline2    end")
) ;let

;; 1.1 未经 bracketed marker 包装的多行字符串同样按行插入
(let ((ed (make-editor "")))
  (check (component-handle-input ed "line1\nline2") => #t)
  (check (editor-text ed) => "line1\nline2")
  (check (editor-lines ed) => '("line1" "line2"))
  (check (editor-cursor ed) => '(1 5))
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

;; 7.2 单行模式下，多行缓冲区仍可用上下方向键移动光标
(let ((ed (make-editor "a\nb" :multiline #f)))
  (editor-set-cursor! ed 1 1)
  (check (component-handle-input ed 'up) => #t)
  (check (editor-cursor ed) => '(0 1))
  (check (component-handle-input ed 'down) => #t)
  (check (editor-cursor ed) => '(1 1))
) ;let

;; 7.3 展开的多行缓冲区：左右键可跨行移动
(let ((ed (make-editor "ab\ncd" :multiline #f)))
  (editor-set-cursor! ed 0 2)
  (check (component-handle-input ed 'right) => #t)
  (check (editor-cursor ed) => '(1 0))
  (check (component-handle-input ed 'left) => #t)
  (check (editor-cursor ed) => '(0 2))
) ;let

;; 7.4 行首退格 / 行尾 Delete 可合并相邻行
(let ((ed (make-editor "ab\ncd" :multiline #f)))
  (editor-set-cursor! ed 1 0)
  (check (component-handle-input ed 'backspace) => #t)
  (check (editor-text ed) => "abcd")
  (check (editor-cursor ed) => '(0 2))
) ;let
(let ((ed (make-editor "ab\ncd" :multiline #f)))
  (editor-set-cursor! ed 0 2)
  (check (component-handle-input ed 'delete) => #t)
  (check (editor-text ed) => "abcd")
  (check (editor-cursor ed) => '(0 2))
) ;let

;; 8. 空粘贴不改变内容
(let ((ed (make-editor "x")))
  (check (component-handle-input ed (paste-event "")) => #t)
  (check (editor-text ed) => "x")
) ;let

(check-report)
