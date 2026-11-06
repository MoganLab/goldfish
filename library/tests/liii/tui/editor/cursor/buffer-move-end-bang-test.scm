;; buffer-move-end!
;; 将光标移动到当前行的行尾。
;;
;; 语法
;; ----
;; (buffer-move-end! buf)
;;
;; 参数
;; ----
;; buf : buffer?
;; 缓冲区实例。
;;
;; 返回值
;; ----
;; #t
;; 操作始终返回 #t。

(import (scheme base)
  (liii check)
  (liii tui editor buffer)
  (liii tui editor cursor)
) ;import

(check-set-mode! 'report-failed)

(let ((b (make-buffer "hello" #f)))
  (check (buffer-move-end! b) => #t)
  (check (buffer-col b) => 5)
) ;let

(check-report)
