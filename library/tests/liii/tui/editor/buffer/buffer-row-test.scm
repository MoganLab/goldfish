;; buffer-row
;; 获取缓冲区光标所在的当前行号（0-based）。
;;
;; 语法
;; ----
;; (buffer-row b)
;;
;; 参数
;; ----
;; b : buffer?
;; 缓冲区实例。
;;
;; 返回值
;; ----
;; integer?
;; 光标当前行号。

(import (scheme base) (liii check) (liii tui editor buffer))

(check-set-mode! 'report-failed)

(let ((b (make-buffer "one\ntwo" #t)))
  (check (buffer-row b) => 0)
) ;let

(check-report)
