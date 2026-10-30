(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 输入区软件方块光标（对齐上游 fake cursor 设计）：
;; 编辑器在光标处绘制反显软件光标（视觉为方块），
;; 硬件光标定位到插入点但保持隐藏（IME 候选窗定位用），
;; 与终端默认光标形状（Windows beam / Linux block）无关。

;; 输入区光标处出现反显软件光标，硬件光标隐藏
(let* ((vterm (make-virtual-terminal :width 40 :height 20))
       (app (make-pi-app :terminal vterm :handler (lambda (app msg) #t)))
       (ed (pi-app-editor app))
      ) ;
  (editor-set-text! ed "abc")
  (editor-set-cursor! ed 0 1)
  (pi-app-render! app)
  ;; 软件光标：光标列处的字符被反显包裹
  (let ((line (find (lambda (l) (string-contains? l "a\x1b;[7mb\x1b;[0mc"))
                 (alt-screen-lines (pi-app-screen app))
               ) ;find
        ) ;line
    ) ;let
    (check-true (string? line))
  ) ;let
  ;; 硬件光标定位到插入点（"a" 后的列 1），但保持隐藏
  (check (terminal-cursor-visible? vterm) => #f)
  (check (terminal-cursor vterm) => '(1 16))
) ;let*

;; 空输入：反显空格作为方块光标
(let* ((vterm (make-virtual-terminal :width 40 :height 20))
       (app (make-pi-app :terminal vterm :handler (lambda (app msg) #t)))
       (ed (pi-app-editor app))
      ) ;
  (pi-app-render! app)
  ;; 空行光标渲染为反显空格
  (let ((line (find (lambda (l) (string-contains? l "\x1b;[7m \x1b;[0m"))
                 (alt-screen-lines (pi-app-screen app))
               ) ;find
        ) ;line
    ) ;let
    (check-true (string? line))
  ) ;let
  (check (terminal-cursor-visible? vterm) => #f)
) ;let*

(check-report)
