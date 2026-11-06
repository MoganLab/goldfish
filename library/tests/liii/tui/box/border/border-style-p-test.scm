;; border-style?
;; 判断给定对象是否为边框风格实例。
;;
;; 语法
;; ----
;; (border-style? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为边框风格实例返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui box border)
) ;import

(check-set-mode! 'report-failed)

(let ((b (border-style-resolve 'single)))
  (check-true (border-style? b))
) ;let

(check-false (border-style? "not-a-style"))
(check-false (border-style? 123))

(check-report)
