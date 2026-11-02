;; %render-cache-lines
;; 获取渲染缓存实例当前保存的行列表。
;;
;; 语法
;; ----
;; (%render-cache-lines cache)
;;
;; 参数
;; ----
;; cache : render-cache?
;; 渲染缓存实例。
;;
;; 返回值
;; ----
;; list?
;; 缓存的行列表。

(import (scheme base) (liii check) (liii pi code modes interactive views cache))

(check-set-mode! 'report-failed)

(let ((c (%make-render-cache "k" '("a" "b"))))
  (check (%render-cache-lines c) => '("a" "b"))
) ;let

(check-report)
