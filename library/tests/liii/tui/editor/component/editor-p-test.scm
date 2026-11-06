;; editor?
;; 判断给定对象是否为编辑器组件。
;;
;; 语法
;; ----
;; (editor? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为编辑器组件返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ed (make-editor)))
  (check-true (editor? ed))
) ;let

(check-false (editor? "not-an-editor"))
(check-false (editor? 123))

(check-report)
