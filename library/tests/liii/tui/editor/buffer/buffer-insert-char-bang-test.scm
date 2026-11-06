;; buffer-insert-char!
;; 在光标当前位置插入单个字符，并将光标右移一列。
;;
;; 语法
;; ----
;; (buffer-insert-char! b ch)
;;
;; 参数
;; ----
;; b : buffer?
;; 缓冲区实例。
;;
;; ch : char?
;; 待插入的字符。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui editor buffer))

(check-set-mode! 'report-failed)

(let ((b (make-buffer "" #f)))
  (buffer-insert-char! b #\a)
  (buffer-insert-char! b #\b)
  (check (buffer-text b) => "ab")
  (check (buffer-col b) => 2)
) ;let

(check-report)
