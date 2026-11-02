;; model-selector-search
;; 获取模型选择器内部的搜索输入框组件实例。
;;
;; 语法
;; ----
;; (model-selector-search comp)
;;
;; 参数
;; ----
;; comp : component?
;; 模型选择器组件。
;;
;; 返回值
;; ----
;; component?
;; 内部搜索输入编辑器组件。

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
             "gemini"
           ) ;make-model-selector
       ) ;ms
       (search-ed (model-selector-search ms))
      ) ;
  (check-true (component? search-ed))
  (check (editor-text search-ed) => "gemini")
) ;let*

(check-report)
