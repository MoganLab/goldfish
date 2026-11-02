;; %vstack-handle-input
;; 顺序向子组件链分发键盘输入，首个消费（返回非 #f）的子组件中止传递。
;;
;; 语法
;; ----
;; (%vstack-handle-input children key)
;;
;; 参数
;; ----
;; children : list?
;; 子组件列表。
;;
;; key : any
;; 按键输入。
;;
;; 返回值
;; ----
;; any
;; 消费子组件的返回值；未被任何子组件消费时返回 #f。

(import (scheme base) (liii check) (liii tui component) (liii tui box vstack))

(check-set-mode! 'report-failed)

(let* ((c1 (make-text "static"))
       (c2
         (make-component
           :render       (lambda (w) '())
           :handle-input (lambda (s k) (if (eq? k 'enter) 'done #f))
         ) ;make-component
       ) ;c2
      ) ;
  (check (%vstack-handle-input (list c1 c2) 'up) => #f)
  (check (%vstack-handle-input (list c1 c2) 'enter) => 'done)
) ;let*

(check-report)
