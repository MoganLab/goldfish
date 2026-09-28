(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; fiber-chan?
;; 判断对象是否为 fiber 通道。
;;
;; 语法
;; ----
;; (fiber-chan? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意对象。
;;
;; 返回值
;; ----
;; boolean
;; obj 是 fiber 通道返回 #t，否则返回 #f。

(check (fiber-chan? (make-fiber-chan)) => #t)
(check (fiber-chan? (make-chan)) => #f)
(check (fiber-chan? 42) => #f)

(check-report)
