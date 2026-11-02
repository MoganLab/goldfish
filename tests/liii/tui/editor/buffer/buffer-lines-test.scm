;; buffer-lines
;; 获取编辑缓冲区中的全部行文本列表。
;;
;; 语法
;; ----
;; (buffer-lines b)
;;
;; 参数
;; ----
;; b : buffer?
;; 缓冲区实例。
;;
;; 返回值
;; ----
;; list?
;; 字符串行列表。

(import (scheme base) (liii check) (liii tui editor buffer))

(check-set-mode! 'report-failed)

(let ((b (make-buffer "one\ntwo" #t)))
  (check (buffer-lines b) => '("one" "two"))
) ;let

(check-report)
