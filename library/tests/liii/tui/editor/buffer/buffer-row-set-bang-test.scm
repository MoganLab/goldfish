;; buffer-row-set!
;; 显式设置光标当前行号。
;;
;; 语法
;; ----
;; (buffer-row-set! b row)
;;
;; 参数
;; ----
;; b : buffer?
;; 缓冲区实例。
;;
;; row : integer?
;; 目标行号。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui editor buffer))

(check-set-mode! 'report-failed)

(let ((b (make-buffer "a\nb" #t)))
  (buffer-row-set! b 1)
  (check (buffer-row b) => 1)
) ;let

(check-report)
