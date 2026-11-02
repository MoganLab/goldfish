;; pi-footer?
;; 判定对象是否为底部状态栏（Footer）组件实例。
;;
;; 语法
;; ----
;; (pi-footer? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检验的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 若为 footer 组件返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive footer)
) ;import

(check-set-mode! 'report-failed)

;; 1. make-pi-footer 构造的实例判定为 #t
(check-true (pi-footer? (make-pi-footer)))

;; 2. 非 footer 组件判定为 #f
(check-false
  (pi-footer? (make-component :render (lambda (s w) '())))
) ;check-false
(check-false (pi-footer? "footer"))
(check-false (pi-footer? #f))
(check-false (pi-footer? 123))

(check-report)
