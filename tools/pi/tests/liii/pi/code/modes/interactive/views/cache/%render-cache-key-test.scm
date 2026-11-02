;; %render-cache-key
;; 获取渲染缓存实例当前的缓存键。
;;
;; 语法
;; ----
;; (%render-cache-key cache)
;;
;; 参数
;; ----
;; cache : render-cache?
;; 渲染缓存实例。
;;
;; 返回值
;; ----
;; any
;; 缓存键对象。

(import (scheme base) (liii check) (liii pi code modes interactive views cache))

(check-set-mode! 'report-failed)

(let ((c (%make-render-cache "my-key" '("row1"))))
  (check (%render-cache-key c) => "my-key")
) ;let

(check-report)
