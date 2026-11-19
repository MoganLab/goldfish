;; buffer-multiline?
;; 查询缓冲区是否处于多行编辑模式。
;;
;; 语法
;; ----
;; (buffer-multiline? b)
;;
;; 参数
;; ----
;; b : buffer?
;; 缓冲区实例。
;;
;; 返回值
;; ----
;; boolean?
;; 支持多行返回 #t，单行返回 #f。

(import (scheme base) (liii check) (liii tui editor buffer))

(check-set-mode! 'report-failed)

(check-false (buffer-multiline? (make-buffer "" #f)))
(check-true (buffer-multiline? (make-buffer "" #t)))

(check-report)
