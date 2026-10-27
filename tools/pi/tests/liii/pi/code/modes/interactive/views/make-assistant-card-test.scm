(import (scheme base) (liii check) (liii string) (liii tui) (liii pi code modes interactive views))

(check-set-mode! 'report-failed)

;; 助手消息透明无背景底色
(let* ((assistant-card (make-assistant-card "这是模型的回答文本。"))
       (w 40)
       (lines (component-render assistant-card w)))
  (check (component? assistant-card) => #t)
  (check (pair? lines) => #t)
  (check (string-contains? (car lines) "\x1b;[48;2;") => #f)
  (check (string-contains? (car lines) "这是模型的回答文本。") => #t))

(check-report)
