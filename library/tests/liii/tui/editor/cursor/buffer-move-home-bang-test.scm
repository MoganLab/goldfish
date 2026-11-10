;; buffer-move-home!
;; 将光标移动到当前行的行首（第 0 列）。
;;
;; 语法
;; ----
;; (buffer-move-home! buf)
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
  (buffer-col-set! b 3)
  (check (buffer-move-home! b) => #t)
  (check (buffer-col b) => 0)
) ;let

(check-report)
