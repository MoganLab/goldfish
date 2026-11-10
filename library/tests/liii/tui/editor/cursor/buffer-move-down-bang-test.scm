;; buffer-move-down!
;; 将光标向下移动一行，保持或钳位列坐标。
;;
;; 语法
;; ----
;; (buffer-move-down! buf)
;;
;; 参数
;; ----
;; buf : buffer?
;; 缓冲区实例。
;;
;; 返回值
;; ----
;; boolean?
;; 移动成功返回 #t，已在末行返回 #f。

(import (scheme base)
  (liii check)
  (liii tui editor buffer)
  (liii tui editor cursor)
) ;import

(check-set-mode! 'report-failed)

(let ((b (make-buffer "abc\ndef" #t)))
  (buffer-col-set! b 2)
  (check-true (buffer-move-down! b))
  (check (buffer-row b) => 1)
  (check (buffer-col b) => 2)
  (check-false (buffer-move-down! b))
) ;let

;; 目标行较短时自动钳位
(let ((b (make-buffer "hello\na" #t)))
  (buffer-col-set! b 4)
  (check-true (buffer-move-down! b))
  (check (buffer-row b) => 1)
  (check (buffer-col b) => 1)
) ;let

(check-report)
