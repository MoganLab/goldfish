;; buffer-lines-set!
;; 显式替换缓冲区中的行列表。
;;
;; 语法
;; ----
;; (buffer-lines-set! b lines)
;;
;; 参数
;; ----
;; b : buffer?
;; 缓冲区实例。
;;
;; lines : list?
;; 目标字符串行列表。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui editor buffer))

(check-set-mode! 'report-failed)

(let ((b (make-buffer "" #t)))
  (buffer-lines-set! b '("a" "b"))
  (check (buffer-lines b) => '("a" "b"))
) ;let

(check-report)
