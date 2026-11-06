;; make-buffer
;; 创建文本编辑缓冲区实例。
;;
;; 语法
;; ----
;; (make-buffer [text] [multiline?])
;;
;; 参数
;; ----
;; text : string? (可选，默认为 "")
;; 初始文本内容。
;;
;; multiline? : boolean? (可选，默认为 #f)
;; 是否支持多行编辑。
;;
;; 返回值
;; ----
;; buffer?
;; 文本编辑缓冲区实例。

(import (scheme base) (liii check) (liii tui editor buffer))

(check-set-mode! 'report-failed)

(let ((b (make-buffer)))
  (check (buffer-lines b) => '(""))
  (check (buffer-row b) => 0)
  (check (buffer-col b) => 0)
  (check (buffer-text b) => "")
  (check-true (buffer-multiline? b))
) ;let

(let ((b (make-buffer "a\nb" #f)))
  (check (buffer-lines b) => '("a" "b"))
  (check-false (buffer-multiline? b))
) ;let

(check-report)
