;; buffer-text
;; 将缓冲区行列表拼接为换行符分隔的完整文本字符串。
;;
;; 语法
;; ----
;; (buffer-text b)
;;
;; 参数
;; ----
;; b : buffer?
;; 缓冲区实例。
;;
;; 返回值
;; ----
;; string?
;; 拼接后的完整文本字符串。

(import (scheme base) (liii check) (liii tui editor buffer))

(check-set-mode! 'report-failed)

(let ((b (make-buffer "one\ntwo" #t)))
  (check (buffer-text b) => "one\ntwo")
) ;let

(check-report)
