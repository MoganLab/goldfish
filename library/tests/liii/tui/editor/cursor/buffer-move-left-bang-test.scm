;; buffer-move-left!
;; 将光标向左移动一列，行首时光标折回上一行末尾。
;;
;; 语法
;; ----
;; (buffer-move-left! buf)
;;
;; 参数
;; ----
;; buf : buffer?
;; 缓冲区实例。
;;
;; 返回值
;; ----
;; boolean?
;; 移动成功返回 #t，处于全文起点返回 #f。

(import (scheme base)
  (liii check)
  (liii tui editor buffer)
  (liii tui editor cursor)
) ;import

(check-set-mode! 'report-failed)

(let ((b (make-buffer "abc" #f)))
  (buffer-col-set! b 2)
  (check-true (buffer-move-left! b))
  (check (buffer-col b) => 1)
  (check-true (buffer-move-left! b))
  (check (buffer-col b) => 0)
  (check-false (buffer-move-left! b))
) ;let

(let ((b (make-buffer "a\nb" #t)))
  (buffer-row-set! b 1)
  (buffer-col-set! b 0)
  ;; 从第二行首左移折回第一行末
  (check-true (buffer-move-left! b))
  (check (buffer-row b) => 0)
  (check (buffer-col b) => 1)
) ;let

(check-report)
