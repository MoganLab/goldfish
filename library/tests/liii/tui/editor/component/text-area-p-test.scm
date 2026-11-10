;; text-area?
;; 判断给定对象是否为多行文本区域组件。
;;
;; 语法
;; ----
;; (text-area? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为多行文本区域组件返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ta (make-text-area)))
  (check-true (text-area? ta))
) ;let

(check-true (text-area? (make-text-input)))
(check-false (text-area? "not-an-area"))

(check-report)
