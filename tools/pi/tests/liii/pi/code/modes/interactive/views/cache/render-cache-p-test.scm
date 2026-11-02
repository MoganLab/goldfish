;; render-cache?
;; 判定对象是否为渲染缓存实例。
;;
;; 语法
;; ----
;; (render-cache? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检验的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 满足条件返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi code modes interactive views cache))

(check-set-mode! 'report-failed)

;; 1. %make-render-cache 构造对象返回 #t
(check-true (render-cache? (%make-render-cache #f '())))

;; 2. 非缓存对象返回 #f
(check-false (render-cache? #f))
(check-false (render-cache? '()))
(check-false (render-cache? 123))

(check-report)
