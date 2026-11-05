;; pi-text-block?
;; 判断对象是否为文本内容块 record 实例。
;;
;; 语法
;; ----
;; (pi-text-block? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于文本内容块 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-text-block "Hello" #f)))
  (check-true (pi-text-block? obj))
) ;let
(check-false (pi-text-block? '()))
(check-false (pi-text-block? #f))
(check-false (pi-text-block? "not-a-record"))

(check-report)
