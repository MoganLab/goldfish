;; buffer-move-word-right!
;; 将光标向右跳转到下一个单词边界。
;;
;; 语法
;; ----
;; (buffer-move-word-right! buf)
;;
;; 参数
;; ----
;; buf : buffer?
;; 缓冲区实例。
;;
;; 返回值
;; ----
;; boolean?
;; 移动成功返回 #t，已在行末返回 #f。

(import (scheme base)
  (liii check)
  (liii tui editor buffer)
  (liii tui editor cursor)
) ;import

(check-set-mode! 'report-failed)

(let ((b (make-buffer "foo bar baz" #f)))
  (check-true (buffer-move-word-right! b))
  (check (buffer-col b) => 4)
  (check-true (buffer-move-word-right! b))
  (check (buffer-col b) => 8)
  (check-true (buffer-move-word-right! b))
  (check (buffer-col b) => 11)
  (check-false (buffer-move-word-right! b))
) ;let

(check-report)
