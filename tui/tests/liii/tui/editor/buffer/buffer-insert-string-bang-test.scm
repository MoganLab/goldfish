;; buffer-insert-string!
;; 在光标当前位置插入单行字符串（不含换行符），并将光标右移相应字符数。
;;
;; 语法
;; ----
;; (buffer-insert-string! b str)
;;
;; 参数
;; ----
;; b : buffer?
;; 缓冲区实例。
;;
;; str : string?
;; 待插入的字符串。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui editor buffer))

(check-set-mode! 'report-failed)

(let ((b (make-buffer "" #f)))
  (buffer-insert-string! b "hello")
  (check (buffer-text b) => "hello")
  (check (buffer-col b) => 5)
) ;let

;; 插入 CJK 字符串
(let ((b (make-buffer "" #f)))
  (buffer-insert-string! b "金鱼")
  (check (buffer-text b) => "金鱼")
  (check (buffer-col b) => 2)
) ;let

(check-report)
