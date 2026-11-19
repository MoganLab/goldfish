;; buffer-kill-line-right!
;; 删除光标位置到行尾的内容（Ctrl-K）；在行尾时合并下一行。
;;
;; 语法
;; ----
;; (buffer-kill-line-right! buf)
;;
;; 参数
;; ----
;; buf : buffer?
;; 缓冲区实例。
;;
;; 返回值
;; ----
;; boolean?
;; 成功删减内容或合并行返回 #t，已在全文末尾返回 #f。

(import (scheme base)
  (liii check)
  (liii tui editor buffer)
  (liii tui editor kill)
) ;import

(check-set-mode! 'report-failed)

(let ((b (make-buffer "hello world" #f)))
  (buffer-col-set! b 5)
  (check-true (buffer-kill-line-right! b))
  (check (buffer-text b) => "hello")
  (check-false (buffer-kill-line-right! b))
) ;let

(let ((b (make-buffer "line1\nline2" #t)))
  (buffer-col-set! b 5)
  ;; 在第一行末尾执行：合并下一行
  (check-true (buffer-kill-line-right! b))
  (check (buffer-text b) => "line1line2")
) ;let

(check-report)
