;; buffer-set-text!
;; 整体重置缓冲区的文本内容并将光标移至末尾。
;;
;; 语法
;; ----
;; (buffer-set-text! b text)
;;
;; 参数
;; ----
;; b : buffer?
;; 缓冲区实例。
;;
;; text : string?
;; 待替换的全文文本。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui editor buffer))

(check-set-mode! 'report-failed)

(let ((b (make-buffer "old" #f)))
  (buffer-set-text! b "hello")
  (check (buffer-text b) => "hello")
  (check (buffer-col b) => 0)
) ;let

(check-report)
