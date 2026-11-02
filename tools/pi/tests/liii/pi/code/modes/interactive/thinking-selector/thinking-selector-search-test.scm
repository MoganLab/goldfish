;; thinking-selector-search
;; 获取思维链选择器内部的搜索框组件。
;;
;; 语法
;; ----
;; (thinking-selector-search comp)
;;
;; 参数
;; ----
;; comp : component?
;; 思维链选择器组件实例。
;;
;; 返回值
;; ----
;; component?
;; 内部搜索编辑器组件。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive thinking-selector)
) ;import

(check-set-mode! 'report-failed)

(let* ((ts (make-thinking-selector "off"
             '("off" "low" "high")
             "off"
             (lambda (l) #t)
             (lambda () #t)
             (lambda (l) #t)
           ) ;make-thinking-selector
       ) ;ts
       (ed (thinking-selector-search ts))
      ) ;
  (check-true (component? ed))
) ;let*

(check-report)
