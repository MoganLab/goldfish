;; buffer-move-up!
;; 将光标向上移动一行，保持或钳位列坐标。
;;
;; 语法
;; ----
;; (buffer-move-up! buf)
;;
;; 参数
;; ----
;; buf : buffer?
;; 缓冲区实例。
;;
;; 返回值
;; ----
;; boolean?
;; 移动成功返回 #t，已在首行返回 #f。

(import (scheme base)
  (liii check)
  (liii tui editor buffer)
  (liii tui editor cursor)
) ;import

(check-set-mode! 'report-failed)

(let ((b (make-buffer "abc\ndef" #t)))
  (buffer-row-set! b 1)
  (buffer-col-set! b 2)
  (check-true (buffer-move-up! b))
  (check (buffer-row b) => 0)
  (check (buffer-col b) => 2)
  (check-false (buffer-move-up! b))
) ;let

;; 目标行较短时自动钳位
(let ((b (make-buffer "a\nhello" #t)))
  (buffer-row-set! b 1)
  (buffer-col-set! b 4)
  (check-true (buffer-move-up! b))
  (check (buffer-row b) => 0)
  (check (buffer-col b) => 1)
) ;let

(check-report)
