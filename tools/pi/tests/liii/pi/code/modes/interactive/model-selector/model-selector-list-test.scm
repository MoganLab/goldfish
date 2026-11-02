;; model-selector-list
;; 获取模型选择器内部的下拉列表组件实例。
;;
;; 语法
;; ----
;; (model-selector-list comp)
;;
;; 参数
;; ----
;; comp : component?
;; 模型选择器组件。
;;
;; 返回值
;; ----
;; component?
;; 内部模型列表选择组件。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive model-selector)
) ;import

(check-set-mode! 'report-failed)

(define %models
  (list (list (cons 'provider "google")
          (cons 'id "gemini-2.5-pro")
          (cons 'name "Gemini 2.5 Pro")
        ) ;list
  ) ;list
) ;define

(let* ((ms (make-model-selector (car %models)
             (car %models)
             %models
             (lambda (m) #t)
             (lambda () #t)
             (lambda (m) #t)
           ) ;make-model-selector
       ) ;ms
       (lst (model-selector-list ms))
      ) ;
  (check-true (component? lst))
) ;let*

(check-report)
