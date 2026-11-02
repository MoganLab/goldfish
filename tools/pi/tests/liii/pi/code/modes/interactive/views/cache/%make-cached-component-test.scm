;; %make-cached-component
;; 构造基于自身渲染缓存的 TUI 组件。
;;
;; 语法
;; ----
;; (%make-cached-component key render-lines)
;;
;; 参数
;; ----
;; key : any
;; 基础缓存标识。
;; render-lines : procedure?
;; 接受 (width) 的行渲染函数。
;;
;; 返回值
;; ----
;; component?
;; 包装了内部缓存的组件。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive views cache)
) ;import

(check-set-mode! 'report-failed)

(let* ((count 0)
       (comp
         (%make-cached-component "item"
           (lambda (w) (set! count (+ count 1)) (list "line"))
         ) ;%make-cached-component
       ) ;comp
      ) ;
  (check (component-render comp 80) => '("line"))
  (check count => 1)
  ;; 宽度相同复用缓存
  (check (component-render comp 80) => '("line"))
  (check count => 1)
) ;let*

(check-report)
