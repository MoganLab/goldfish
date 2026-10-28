(import (scheme base)
  (liii check)
  (liii unicode)
  (liii tui)
) ;import

(check-set-mode! 'report)

(define (paste-event text)
  (string-append "\x1b;[200~" text "\x1b;[201~")
) ;define

;; 1. 小段多行粘贴直接展开，不触发提交
(let ((ed (make-editor "" :multiline #t)))
  (check (component-handle-input ed (paste-event "line1\r\nline2\tend")) => #t)
  (check (editor-text ed) => "line1\nline2    end")
  (check (editor-lines ed) => '("line1" "line2    end"))
  (check (editor-cursor ed) => '(1 12))
  (check (editor-expanded-text ed) => "line1\nline2    end")
) ;let

;; 1.1 在行中间粘贴：拆分当前行，保留光标前后内容
(let ((ed (make-editor "hello" :multiline #t)))
  (editor-set-cursor! ed 0 5)
  (component-handle-input ed (paste-event "X\nY"))
  (check (editor-lines ed) => '("helloX" "Y"))
  (check (editor-cursor ed) => '(1 1))
) ;let

;; 1.2 以换行结尾的粘贴保留末尾空行
(let ((ed (make-editor "" :multiline #t)))
  (component-handle-input ed (paste-event "a\n"))
  (check (editor-lines ed) => '("a" ""))
  (check (editor-cursor ed) => '(1 0))
) ;let

;; 2. 超过 10 行折叠为行数标记
(let* ((big (string-append "a" (make-string 11 #\newline) "b"))
       (ed (make-editor "" :multiline #t))
      ) ;
  (component-handle-input ed (paste-event big))
  (check (editor-text ed) => "[paste #1 +12 lines]")
  (check (editor-lines ed) => '("[paste #1 +12 lines]"))
  (check (editor-expanded-text ed) => big)
) ;let*

;; 3. 超过 1000 字符（单行）折叠为字符数标记
(let* ((big (make-string 1200 #\a))
       (ed (make-editor "" :multiline #t))
      ) ;
  (component-handle-input ed (paste-event big))
  (check (editor-text ed) => "[paste #1 1200 chars]")
  (check (editor-expanded-text ed) => big)
) ;let*

;; 4. 多行且超过 1000 字符：字符数优先于行数
(let* ((big (string-append (make-string 1200 #\a) (make-string 20 #\newline)))
       (ed (make-editor "" :multiline #t))
      ) ;
  (component-handle-input ed (paste-event big))
  (check (editor-text ed) => "[paste #1 1220 chars]")
  (check (editor-expanded-text ed) => big)
) ;let*

;; 5. 提交时展开真实内容
(let* ((big (make-string 1200 #\a))
       (submitted #f)
       (ed (make-editor ""
             :multiline #t
             :on-submit (lambda (comp text) (set! submitted text))
           ) ;make-editor
       ) ;
      ) ;
  (component-handle-input ed (paste-event big))
  (check (component-handle-input ed 'enter) => #t)
  (check submitted => big)
) ;let*

;; 6. Backspace 整体删除标记；编号单调递增不复用
(let* ((big (make-string 1200 #\a))
       (ed (make-editor "" :multiline #t))
      ) ;
  (component-handle-input ed (paste-event big))
  (check (component-handle-input ed 'backspace) => #t)
  (check (editor-text ed) => "")
  (component-handle-input ed (paste-event big))
  (check (editor-text ed) => "[paste #2 1200 chars]")
) ;let*

;; 7. Delete 整体删除光标处的标记
(let* ((big (make-string 1200 #\a))
       (ed (make-editor "" :multiline #t))
      ) ;
  (component-handle-input ed (paste-event big))
  (editor-set-cursor! ed 0 0)
  (check (component-handle-input ed 'delete) => #t)
  (check (editor-text ed) => "")
) ;let*

;; 8. editor-set-text! 清空注册表并重置编号
(let* ((big (make-string 1200 #\a))
       (ed (make-editor "" :multiline #t))
      ) ;
  (component-handle-input ed (paste-event big))
  (editor-set-text! ed "")
  (component-handle-input ed (paste-event big))
  (check (editor-text ed) => "[paste #1 1200 chars]")
) ;let*

;; 9. 小段粘贴会触发 on-change，大段粘贴的 on-change 看到的是标记文本
(let* ((seen #f)
       (ed (make-editor ""
             :multiline #t
             :on-change (lambda (comp text) (set! seen text))
           ) ;make-editor
       ) ;
      ) ;
  (component-handle-input ed (paste-event "x"))
  (check seen => "x")
  (component-handle-input ed (paste-event (make-string 1200 #\a)))
  (check seen => "x[paste #1 1200 chars]")
) ;let*

;; 10. 历史导航整体替换缓冲区后旧标记失效：手打的标记文本按字面提交
(let* ((big (make-string 1200 #\a))
       (submitted #f)
       (ed (make-editor ""
             :on-submit (lambda (comp text) (set! submitted text))
           ) ;make-editor
       ) ;
      ) ;
  (component-handle-input ed "h")
  (component-handle-input ed 'enter)
  (check submitted => "h")
  (editor-set-text! ed "")
  (component-handle-input ed (paste-event big))
  (check (editor-text ed) => "[paste #1 1200 chars]")
  ;; up 加载历史：整体替换缓冲区，注册表随之清空
  (check (component-handle-input ed 'up) => #t)
  (check (editor-text ed) => "h")
  ;; 手打与旧标记相同的文本：不再被展开
  (let ((fake "[paste #1 1200 chars]"))
    (let loop ((i 0))
      (when (< i (string-length fake))
        (component-handle-input ed (substring fake i (+ i 1)))
        (loop (+ i 1))
      ) ;when
    ) ;let
  ) ;let
  (component-handle-input ed 'enter)
  (check submitted => "h[paste #1 1200 chars]")
) ;let*

;; 11. 历史往返不丢粘贴内容：草稿保存的是展开后的文本
(let* ((big (make-string 1200 #\a))
       (ed (make-editor "" :on-submit (lambda (comp text) #f)))
      ) ;
  (component-handle-input ed "h")
  (component-handle-input ed 'enter)
  (editor-set-text! ed "")
  (component-handle-input ed (paste-event big))
  (component-handle-input ed 'up)
  (check (editor-text ed) => "h")
  (component-handle-input ed 'down)
  (check (editor-text ed) => big)
) ;let*

(check-report)
