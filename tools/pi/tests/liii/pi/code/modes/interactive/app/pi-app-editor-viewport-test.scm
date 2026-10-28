(import (scheme base)
  (liii check)
  (liii unicode)
  (liii string)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report)

;; 编辑器可视行随终端高度（1/3）与滚动提示
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (app msg) #t)))
       (screen (pi-app-screen app))
       (ed (pi-app-editor app))
       (text
         (string-join
           (map (lambda (i) (string-append "line " (number->string i))) (iota 20))
           "\n"
         ) ;string-join
       ) ;text
      ) ;
  (editor-set-text! ed text)
  (editor-set-cursor! ed 19 0)
  (pi-app-render! app)
  ;; 40 行终端：内容上限 13 行（1/3 与剩余空间取小）
  (check (editor-viewport ed) => '(7 13 7 0))
  (check (editor-max-lines ed) => 13)
  (let ((plain (map strip-ansi (screen-lines screen))))
    ;; header 6 + 上边框 + 顶部提示 + 13 行内容 + 下边框 + footer 2
    (check (length plain) => 24)
    (check-true (string-contains? (list-ref plain 7) "↑ 7"))
    (check (string-contains? (list-ref plain 8) "line 7") => #t)
    (check (string-contains? (list-ref plain 20) "line 19") => #t)
    (check (list-ref plain 21) => (utf8-make-string 100 #\─))
  ) ;let
  ;; 硬件光标跟随滚动后的可视行（header 6 + 上边框 + 顶部提示 + (19 - 7)）
  (check (terminal-cursor vterm) => '(0 20))
) ;let*

;; 行数不足上限时不出现滚动提示
(let* ((vterm (make-virtual-terminal :width 80 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (app msg) #t)))
       (ed (pi-app-editor app))
      ) ;
  (editor-set-text! ed "a\nb\nc")
  (pi-app-render! app)
  (check (editor-viewport ed) => '(0 3 0 0))
  (check (length (component-render ed 80)) => 5)
) ;let*

;; 极矮终端：预算不低于 5 行
(let* ((vterm (make-virtual-terminal :width 80 :height 12))
       (app (make-pi-app :terminal vterm :handler (lambda (app msg) #t)))
      ) ;
  (pi-app-render! app)
  (check (editor-max-lines (pi-app-editor app)) => 5)
) ;let*

;; 连续换行：窗口填满后继续输入，光标不再向下移动
(let* ((vterm (make-virtual-terminal :width 80 :height 30))
       (app (make-pi-app :terminal vterm :handler (lambda (app msg) #t)))
       (ed (pi-app-editor app))
      ) ;
  (pi-app-render! app)
  (let loop ((i 0))
    (when (< i 10)
      (pi-app-handle-input! app 'ctrl-j)
      (pi-app-render! app)
      (loop (+ i 1))
    ) ;when
  ) ;let
  (let ((filled-y (cadr (terminal-cursor vterm))))
    (let loop ((i 0))
      (when (< i 15)
        (pi-app-handle-input! app 'ctrl-j)
        (pi-app-render! app)
        (loop (+ i 1))
      ) ;when
    ) ;let
    (check (editor-viewport ed) => '(16 10 16 0))
    (check (cadr (terminal-cursor vterm)) => filled-y)
  ) ;let
) ;let*

(check-report)
