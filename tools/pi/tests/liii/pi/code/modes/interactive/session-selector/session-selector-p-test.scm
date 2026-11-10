;; session-selector?
;; 判定对象是否满足 session-selector? 条件。
;;
;; 语法
;; ----
;; (session-selector? ...)
;;
;; 参数
;; ----
;; 详见语法说明。
;;
;; 返回值
;; ----
;; boolean?
;; 满足条件返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code core session-list)
  (liii pi code modes interactive session-selector)
) ;import

(check-set-mode! 'report-failed)

;; session-selector?：判断对象是否为会话选择器组件

(define (info path)
  (make-pi-session-info path path "/work" #f "created"
    "2026-10-21T08:00:01.000Z" 1 path path
  ) ;make-pi-session-info
) ;define

(let ((selector (make-session-selector (list (info "a")) '() (lambda (path) #f) (lambda () #f))
      ) ;selector
     ) ;
  (check-true (session-selector? selector))
  (check-false (session-selector? '()))
  (check-false (session-selector? "not a selector"))
) ;let

(check-report)
