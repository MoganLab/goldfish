;; buffer-col
;; 获取缓冲区光标所在的当前列号（Unicode 字符偏移，0-based）。
;;
;; 语法
;; ----
;; (buffer-col b)
;;
;; 参数
;; ----
;; b : buffer?
;; 缓冲区实例。
;;
;; 返回值
;; ----
;; integer?
;; 光标当前列号。

(import (scheme base) (liii check) (liii tui editor buffer))

(check-set-mode! 'report-failed)

(let ((b (make-buffer "hello" #f)))
  (check (buffer-col b) => 0)
) ;let

(check-report)
