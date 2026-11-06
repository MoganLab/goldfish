;; %vstack-invalidate
;; 递归使子组件列表的所有子组件脏区失效。
;;
;; 语法
;; ----
;; (%vstack-invalidate children)
;;
;; 参数
;; ----
;; children : list?
;; 子组件列表。
;;
;; 返回值
;; ----
;; boolean?
;; 操作成功返回 #t。

(import (scheme base) (liii check) (liii tui component) (liii tui box vstack))

(check-set-mode! 'report-failed)

(let* ((inv-count 0)
       (c1
         (make-component
           :render     (lambda (w) '())
           :invalidate (lambda (s) (set! inv-count (+ inv-count 1)) #t)
         ) ;make-component
       ) ;c1
       (c2
         (make-component
           :render     (lambda (w) '())
           :invalidate (lambda (s) (set! inv-count (+ inv-count 1)) #t)
         ) ;make-component
       ) ;c2
      ) ;
  (check-true (%vstack-invalidate (list c1 c2)))
  (check inv-count => 2)
) ;let*

(check-report)
