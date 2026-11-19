;; buffer-p
;; 判断给定对象是否为文本编辑缓冲区实例。
;;
;; 语法
;; ----
;; (buffer? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为编辑缓冲区实例返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui editor buffer)
) ;import

(check-set-mode! 'report-failed)

(let ((b (make-buffer)))
  (check-true (buffer? b))
) ;let

(check-false (buffer? "not-a-buffer"))
(check-false (buffer? 123))

(check-report)
