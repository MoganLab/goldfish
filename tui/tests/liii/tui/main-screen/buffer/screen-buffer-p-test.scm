;; screen-buffer?
;; 判断给定对象是否为屏幕行缓冲区实例。
;;
;; 语法
;; ----
;; (screen-buffer? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为屏幕行缓冲区实例返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui main-screen buffer)
) ;import

(check-set-mode! 'report-failed)

(let ((buf (make-screen-buffer)))
  (check-true (screen-buffer? buf))
) ;let

(check-false (screen-buffer? "not-a-buffer"))
(check-false (screen-buffer? 123))

(check-report)
