(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
) ;import

(check-set-mode! 'report)

;; 退出 TUI 时，inline 终端必须把光标移到整个块的下方。
;; 回归点：曾用 (screen 行数) 作为绝对 y，但当时光标停在输入行，
;; %st-inline-seq 只在目标 y > max-y 时才补换行，导致 shell prompt
;; 被画在输入框位置。
(let* ((buf (open-output-string))
       (term (make-standard-terminal :output-port buf :inline #t))
      ) ;
  ;; 模拟 TUI 渲染：从第 0 行写到第 10 行，最后光标停在输入行（第 7 行）
  (terminal-set-cursor! term 0 10)
  (terminal-set-cursor! term 0 7)
  (let ((max-y (terminal-max-y term)))
    ;; 到达过的最大行必须是 10，而不是当前光标行 7。
    ;; 这正是旧实现（用 screen 行数 / 当前光标行当绝对 y）会算错的地方
    (check max-y => 10)
  ) ;let
) ;let*

(check-report)
