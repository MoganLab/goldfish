;; %cache-set-lines!
;; 设置渲染缓存的键与行列表，并返回新设置的行列表。
;;
;; 语法
;; ----
;; (%cache-set-lines! cache key lines)
;;
;; 参数
;; ----
;; cache : render-cache?
;; 渲染缓存实例。
;; key : any
;; 新缓存键。
;; lines : list?
;; 新行列表。
;;
;; 返回值
;; ----
;; list?
;; 设置的行列表。

(import (scheme base) (liii check) (liii pi code modes interactive views cache))

(check-set-mode! 'report-failed)

(let ((c (%make-render-cache #f '())))
  (check (%cache-set-lines! c "k2" '("x" "y")) => '("x" "y"))
  (check (%render-cache-key c) => "k2")
  (check (%render-cache-lines c) => '("x" "y"))
) ;let

(check-report)
