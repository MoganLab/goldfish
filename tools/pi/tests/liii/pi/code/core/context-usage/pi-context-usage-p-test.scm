;; pi-context-usage?
;; 判断对象是否为上下文占用 record
;;
;; 语法
;; ----
;; (pi-context-usage? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;;
;; 返回值
;; ----
;; boolean
;; 仅对 pi-context-usage record 返回 #t。

(import (scheme base) (liii check) (liii pi code core context-usage))

(check-set-mode! 'report-failed)

(let ((ctx (make-pi-context-usage 12.3 1048576 #t)))
  (check-true (pi-context-usage? ctx))
  (check (pi-context-usage-percent ctx) => 12.3)
  (check (pi-context-usage-window ctx) => 1048576)
  (check (pi-context-usage-known? ctx) => #t)
) ;let

(check-false (pi-context-usage? #f))
(check-false (pi-context-usage? '()))
(check-false (pi-context-usage? (list (cons 'percent 0.0))))

(check-report)
