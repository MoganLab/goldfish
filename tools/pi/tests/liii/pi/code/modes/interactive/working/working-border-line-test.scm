(import (scheme base)
  (liii check)
  (liii string)
  (liii unicode)
  (liii tui)
  (liii pi code modes interactive working))

(check-set-mode! 'report)

;; 渲染格式：── ⠋ Working ──… 填满整行，颜色由调用方注入
(let ((ind (make-working-indicator)))
  (let* ((line (working-border-line ind 100 "\x1b;[38;2;151;118;229m"))
         (plain (strip-ansi line))
         (dashes (utf8-make-string (- 100 13) #\─)))
    ;; 纯文本格式：── + 空格 + spinner + 空格 + Working + 空格 + 剩余横线
    (check plain => (string-append "── ⠋ Working " dashes))
    ;; 可见宽度恰为终端宽度
    (check (display-width plain) => 100)
    ;; 使用调用方注入的主题色（spinner/文字/边框线同色）
    (check-true (string-contains? line "\x1b;[38;2;151;118;229m"))
    ;; 复位色尾码
    (check-true (string-contains? line "\x1b;[0m")))
  ;; 帧推进后行内容随之变化
  (working-advance! ind)
  (check-true (string-contains? (strip-ansi (working-border-line ind 100 "\x1b;[38;2;151;118;229m")) "⠙")))

(check-report)
