;; model-selector-cursor-position
;; 获取模型选择器当前搜索框的光标相对坐标 (cons row col)。
;;
;; 语法
;; ----
;; (model-selector-cursor-position comp)
;;
;; 参数
;; ----
;; comp : component?
;; 模型选择器组件。
;;
;; 返回值
;; ----
;; pair?
;; 包含光标行列位置的点对 (row . col)。

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
       (pos (model-selector-cursor-position ms))
      ) ;
  (check-true (pair? pos))
  (check (number? (car pos)) => #t)
  (check (number? (cadr pos)) => #t)
) ;let*

(check-report)
