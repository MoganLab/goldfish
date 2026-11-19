;; buffer-col-set!
;; 显式设置光标当前列号。
;;
;; 语法
;; ----
;; (buffer-col-set! b col)
;;
;; 参数
;; ----
;; b : buffer?
;; 缓冲区实例。
;;
;; col : integer?
;; 目标列号。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui editor buffer))

(check-set-mode! 'report-failed)

(let ((b (make-buffer "hello" #f)))
  (buffer-col-set! b 3)
  (check (buffer-col b) => 3)
) ;let

(check-report)
