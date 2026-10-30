(import (scheme base)
  (liii check)
  (liii string)
  (liii tui terminal)
  (liii tui editor)
  (liii tui component)
  (liii tui main-screen)
) ;import

(check-set-mode! 'report-failed)

;; 模拟真实 TUI 路径：虚拟终端 + main-screen + 编辑器组件，
;; 检查内容满行时最后一列是否被写入、硬件光标停在哪一列
(let* ((vt (make-virtual-terminal :width 20 :height 8))
       (scr (make-main-screen vt))
       (ed (make-editor "" :cursor-style 'hardware))
      ) ;let*
  ;; 输入 18 个字符：布局宽度 19，恰好占满第一视觉行的列 0..17
  (for-each
    (lambda (i) (component-handle-input ed (string (integer->char (+ (char->integer #\a) i)))))
    (iota 18)
  ) ;for-each
  (let* ((lines (component-render ed 20))
         (pos (editor-display-position ed 20))
        ) ;let*
    (check (length lines) => 1)
    ;; 内容 18 个字符，小于终端宽度 20：最后一列不被内容写入
    (check (string-length (car lines)) => 18)
    ;; 光标视觉位置：行 0，列 18（0-indexed，仍在终端内）
    (check pos => '(0 18))
  ) ;let*
) ;let*

;; 输入 19 个字符：恰好占满布局宽度 19（列 0..18），不折行，
;; 光标停在预留列 19（组件最后一列，0-indexed）——
;; 修复前光标被钳到列 18 压在最后一个字符上，看不出预留列
(let* ((vt (make-virtual-terminal :width 20 :height 8))
       (scr (make-main-screen vt))
       (ed (make-editor "" :cursor-style 'hardware))
      ) ;let*
  (for-each
    (lambda (i) (component-handle-input ed (string (integer->char (+ (char->integer #\a) i)))))
    (iota 19)
  ) ;for-each
  (let* ((lines (component-render ed 20))
         (pos (editor-display-position ed 20))
        ) ;let*
    (check lines => '("abcdefghijklmnopqrs"))
    (check pos => '(0 19))
  ) ;let*
) ;let*

;; 输入 20 个字符：超出布局宽度 19，第 20 个字符折到下一行
(let* ((vt (make-virtual-terminal :width 20 :height 8))
       (scr (make-main-screen vt))
       (ed (make-editor "" :cursor-style 'hardware))
      ) ;let*
  (for-each
    (lambda (i) (component-handle-input ed (string (integer->char (+ (char->integer #\a) i)))))
    (iota 20)
  ) ;for-each
  (let* ((lines (component-render ed 20))
         (pos (editor-display-position ed 20))
        ) ;let*
    (check lines => '("abcdefghijklmnopqrs" "t"))
    (check pos => '(1 1))
  ) ;let*
) ;let*

(check-report)
