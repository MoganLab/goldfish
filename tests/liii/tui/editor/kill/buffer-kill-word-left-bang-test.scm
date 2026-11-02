;; buffer-kill-word-left!
;; 向左删除一个单词（Ctrl-W / Alt-Backspace）。
;;
;; 语法
;; ----
;; (buffer-kill-word-left! buf)
;;
;; 参数
;; ----
;; buf : buffer?
;; 缓冲区实例。
;;
;; 返回值
;; ----
;; boolean?
;; 成功删除单词返回 #t，处于行首返回 #f。

(import (scheme base)
  (liii check)
  (liii tui editor buffer)
  (liii tui editor kill)
) ;import

(check-set-mode! 'report-failed)

(let ((b (make-buffer "foo bar baz" #f)))
  (buffer-col-set! b 11)
  (check-true (buffer-kill-word-left! b))
  (check (buffer-text b) => "foo bar ")
  (check (buffer-col b) => 8)
) ;let

(check-report)
