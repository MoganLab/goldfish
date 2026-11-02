;; model-selector?
;; 判定对象是否为模型选择器组件实例。
;;
;; 语法
;; ----
;; (model-selector? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检验的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 满足条件返回 #t，否则返回 #f。

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

;; 1. make-model-selector 返回实例判定为 #t
(let ((ms (make-model-selector (car %models)
            (car %models)
            %models
            (lambda (m) #t)
            (lambda () #t)
            (lambda (m) #t)
          ) ;make-model-selector
      ) ;ms
     ) ;
  (check-true (model-selector? ms))
) ;let

;; 2. 非模型选择器判定为 #f
(check-false
  (model-selector? (make-component :render (lambda (s w) '())))
) ;check-false
(check-false (model-selector? "selector"))
(check-false (model-selector? #f))
(check-false (model-selector? 123))

(check-report)
