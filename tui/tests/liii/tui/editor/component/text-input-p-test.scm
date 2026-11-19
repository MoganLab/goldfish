;; text-input?
;; 判断给定对象是否为单行文本输入框组件。
;;
;; 语法
;; ----
;; (text-input? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为单行文本输入框组件返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ti (make-text-input)))
  (check-true (text-input? ti))
) ;let

(check-true (text-input? (make-text-area)))
(check-false (text-input? "not-an-input"))

(check-report)
