(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii tui component)
  (liii pi code modes interactive model-selector)
) ;import

(check-set-mode! 'report-failed)

;; 选择器搜索框软件方块光标：
;; 选择器内嵌的搜索输入框不使用硬件光标样式，
;; 而是在光标处绘制反显软件光标（视觉为方块），
;; 与终端默认光标形状（Windows beam / Linux block）无关。

;; 光标处字符被反显包裹
(let ((selector (make-model-selector
                  (list (cons 'provider "google") (cons 'id "gemini-2.5-pro"))
                  #f
                  '()
                  (lambda (m) #f)
                  (lambda () #f)
                  (lambda (m) #f)
                ) ;make-model-selector
     ) ;selector
    ) ;
  (component-handle-input selector "g")
  (component-handle-input selector "m")
  ;; 光标在行尾（"m" 之后）：反显空格作方块光标
  (let* ((lines (component-render selector 100))
         (line (find (lambda (l) (string-contains? l "gm\x1b;[7m \x1b;[0m")) lines))
        ) ;let
    (check-true (string? line))
  ) ;let
) ;let*

(check-report)
