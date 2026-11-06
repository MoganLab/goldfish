;; buffer-transpose-chars!
;; 交换光标前两个字符（Ctrl-T）。
;;
;; 语法
;; ----
;; (buffer-transpose-chars! buf)
;;
;; 参数
;; ----
;; buf : buffer?
;; 缓冲区实例。
;;
;; 返回值
;; ----
;; boolean?
;; 成功交换返回 #t，光标前不足两个字符时返回 #f。

(import (scheme base)
  (liii check)
  (liii tui editor buffer)
  (liii tui editor kill)
) ;import

(check-set-mode! 'report-failed)

(let ((b (make-buffer "ab" #f)))
  (buffer-col-set! b 2)
  (check-true (buffer-transpose-chars! b))
  (check (buffer-text b) => "ba")
) ;let

(let ((b (make-buffer "a" #f)))
  (buffer-col-set! b 1)
  (check-false (buffer-transpose-chars! b))
) ;let

(check-report)
