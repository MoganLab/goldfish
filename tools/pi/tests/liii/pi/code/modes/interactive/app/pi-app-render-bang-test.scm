(import (scheme base)
  (liii check)
  (liii unicode)
  (liii string)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report)

;; pi-app-render!
;; 渲染交互式应用的完整界面，并更新编辑器视口与硬件光标位置。
;;
;; 语法
;; ----
;; (pi-app-render! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 待渲染的交互式应用状态。
;;
;; 返回值
;; ----
;; 未指定
;; 该过程通过副作用将界面写入应用关联的屏幕。
;;
;; 注意
;; ----
;; 编辑器内容按终端宽度折行。长输入使用视觉行滚动，硬件光标跟随
;; 光标所在的视觉行，编辑器下边框不会被内容覆盖。

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
  (let ((plain (map strip-ansi (alt-screen-lines screen))))
    ;; 全屏帧固定 40 行：对话记录视口 22 行 + 上边框 + 顶部提示 + 13 行内容
    ;; + 下边框 + footer 2
    (check (length plain) => 40)
    (check-true (string-contains? (list-ref plain 23) "↑ 7"))
    (check (string-contains? (list-ref plain 24) "line 7") => #t)
    (check (string-contains? (list-ref plain 36) "line 19") => #t)
    (check (list-ref plain 37) => (utf8-make-string 100 #\─))
  ) ;let
  ;; 硬件光标跟随滚动后的可视行（视口 22 + 上边框 + 顶部提示 + (19 - 7)）
  (check (terminal-cursor vterm) => '(0 36))
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

;; 极矮终端：输入区预算取终端高度的 1/3，且给对话记录视口留出空间
(let* ((vterm (make-virtual-terminal :width 80 :height 12))
       (app (make-pi-app :terminal vterm :handler (lambda (app msg) #t)))
      ) ;
  (pi-app-render! app)
  (check (editor-max-lines (pi-app-editor app)) => 4)
) ;let*

;; 连续换行：窗口填满后继续输入，光标不再向下移动
(let* ((vterm (make-virtual-terminal :width 80 :height 30))
       (app (make-pi-app :terminal vterm :handler (lambda (app msg) #t)))
       (ed (pi-app-editor app))
      ) ;
  (pi-app-render! app)
  (let loop
    ((i 0))
    (when (< i 10)
      (pi-app-handle-input! app 'ctrl-j)
      (pi-app-render! app)
      (loop (+ i 1))
    ) ;when
  ) ;let
  (let ((filled-y (cadr (terminal-cursor vterm))))
    (let loop
      ((i 0))
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

;; 长输入按视觉行滚动，光标停留在最后一个视觉行且下边框不被覆盖
(let* ((vterm (make-virtual-terminal :width 10 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (app msg) #t)))
       (ed (pi-app-editor app))
       (screen (pi-app-screen app))
      ) ;
  (editor-set-text! ed "one two three")
  (editor-set-cursor! ed 0 13)
  (pi-app-render! app)
  (check (editor-viewport ed 10) => '(0 2 0 0))
  ;; 全屏布局：视口 34 行 + 上边框 + 第 2 个可视行
  (check (terminal-cursor vterm) => '(5 36))
  (check (list-ref (map strip-ansi (alt-screen-lines screen)) 37)
    =>
    (utf8-make-string 10 #\─)
  ) ;check
) ;let*

(check-report)
