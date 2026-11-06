;; buffer-kill-line-left!
;; 删除光标位置到行首的内容（Ctrl-U）；在行首时合并到上一行末尾。
;;
;; 语法
;; ----
;; (buffer-kill-line-left! buf)
;;
;; 参数
;; ----
;; buf : buffer?
;; 缓冲区实例。
;;
;; 返回值
;; ----
;; boolean?
;; 成功删减内容或合并行返回 #t，已在全文起点返回 #f。

(import (scheme base)
  (liii check)
  (liii tui editor buffer)
  (liii tui editor kill)
) ;import

(check-set-mode! 'report-failed)

(let ((b (make-buffer "hello world" #f)))
  (buffer-col-set! b 5)
  (check-true (buffer-kill-line-left! b))
  (check (buffer-text b) => " world")
  (check (buffer-col b) => 0)
  (check-false (buffer-kill-line-left! b))
) ;let

(let ((b (make-buffer "one\ntwo" #t)))
  (buffer-row-set! b 1)
  (buffer-col-set! b 0)
  ;; 在第二行首执行：与上一行合并
  (check-true (buffer-kill-line-left! b))
  (check (buffer-text b) => "onetwo")
  (check (buffer-row b) => 0)
  (check (buffer-col b) => 3)
) ;let

(check-report)
