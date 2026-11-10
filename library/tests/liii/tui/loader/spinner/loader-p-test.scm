;; loader?
;; 判断给定对象是否为加载器实例（spinner? 的别名）。
;;
;; 语法
;; ----
;; (loader? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为加载器（微调器）实例返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui loader spinner))

(check-set-mode! 'report-failed)

(let ((l (make-loader "test")))
  (check-true (loader? l))
) ;let

(check-false (loader? "not-a-loader"))
(check-false (loader? 123))

(check-report)
