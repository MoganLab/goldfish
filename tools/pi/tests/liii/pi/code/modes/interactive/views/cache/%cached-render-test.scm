;; %cached-render
;; 基于缓存键复用或执行生成渲染行列表。
;;
;; 语法
;; ----
;; (%cached-render cache key render-lines)
;;
;; 参数
;; ----
;; cache : render-cache?
;; 渲染缓存实例。
;; key : any
;; 缓存键。
;; render-lines : procedure?
;; 无参的行渲染函数。
;;
;; 返回值
;; ----
;; list?
;; 渲染行列表。

(import (scheme base) (liii check) (liii pi code modes interactive views cache))

(check-set-mode! 'report-failed)

(let ((c (%make-render-cache #f '())) (counter 0))
  (define (render)
    (set! counter (+ counter 1))
    (list "rendered")
  ) ;define

  ;; 首次计算
  (check (%cached-render c "k1" render) => '("rendered"))
  (check counter => 1)

  ;; 相同 key 命中缓存，不增加计数
  (check (%cached-render c "k1" render) => '("rendered"))
  (check counter => 1)

  ;; 不同 key 重新计算
  (check (%cached-render c "k2" render) => '("rendered"))
  (check counter => 2)
) ;let

(check-report)
