;; assistant-card?
;; 判断对象是否为 <assistant-card> 内部状态记录。
;;
;; 语法
;; ----
;; (assistant-card? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检测的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 若为 <assistant-card> 记录则返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii pi code modes interactive views assistant)
) ;import

(check-set-mode! 'report-failed)

(let ((card (make-assistant-card "hi")))
  (check (assistant-card? (component-data card)) => #t)
  (check (assistant-card? card) => #f)
  (check (assistant-card? "hi") => #f)
) ;let

(check-report)
