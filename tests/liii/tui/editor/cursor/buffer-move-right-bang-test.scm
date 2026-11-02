;; buffer-move-right!
;; 将光标向右移动一列，行末时光标折至下一行首。
;;
;; 语法
;; ----
;; (buffer-move-right! buf)
;;
;; 参数
;; ----
;; buf : buffer?
;; 缓冲区实例。
;;
;; 返回值
;; ----
;; boolean?
;; 移动成功返回 #t，处于全文末尾返回 #f。

(import (scheme base)
  (liii check)
  (liii tui editor buffer)
  (liii tui editor cursor)
) ;import

(check-set-mode! 'report-failed)

(let ((b (make-buffer "ab" #f)))
  (check-true (buffer-move-right! b))
  (check (buffer-col b) => 1)
  (check-true (buffer-move-right! b))
  (check (buffer-col b) => 2)
  (check-false (buffer-move-right! b))
) ;let

(let ((b (make-buffer "a\nb" #t)))
  (buffer-col-set! b 1)
  ;; 从第一行末右移折至第二行首
  (check-true (buffer-move-right! b))
  (check (buffer-row b) => 1)
  (check (buffer-col b) => 0)
) ;let

(check-report)
