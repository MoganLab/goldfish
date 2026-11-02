;; buffer-move-word-left!
;; 将光标向左跳转到上一个单词边界。
;;
;; 语法
;; ----
;; (buffer-move-word-left! buf)
;;
;; 参数
;; ----
;; buf : buffer?
;; 缓冲区实例。
;;
;; 返回值
;; ----
;; boolean?
;; 移动成功返回 #t，已在全文起点返回 #f。

(import (scheme base)
  (liii check)
  (liii tui editor buffer)
  (liii tui editor cursor)
) ;import

(check-set-mode! 'report-failed)

(let ((b (make-buffer "foo bar baz" #f)))
  (buffer-col-set! b 11)
  (check-true (buffer-move-word-left! b))
  (check (buffer-col b) => 8)
  (check-true (buffer-move-word-left! b))
  (check (buffer-col b) => 4)
  (check-true (buffer-move-word-left! b))
  (check (buffer-col b) => 0)
  (check-false (buffer-move-word-left! b))
) ;let

(check-report)
