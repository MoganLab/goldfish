;; %make-render-cache
;; 构造渲染缓存 record 实例。
;;
;; 语法
;; ----
;; (%make-render-cache key lines)
;;
;; 参数
;; ----
;; key : any
;; 缓存键。
;; lines : list?
;; 缓存的渲染行列表。
;;
;; 返回值
;; ----
;; render-cache?
;; 渲染缓存实例。

(import (scheme base) (liii check) (liii pi code modes interactive views cache))

(check-set-mode! 'report-failed)

(let ((c (%make-render-cache 'k1 '("line1"))))
  (check-true (render-cache? c))
  (check (%render-cache-key c) => 'k1)
  (check (%render-cache-lines c) => '("line1"))
) ;let

(check-report)
