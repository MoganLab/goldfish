;; find-define-library
;; 从 S 表达式顶级形式列表中查找 define-library 形式。
;;
;; 语法
;; ----
;; (find-define-library forms)
;;
;; 参数
;; ----
;; forms : list  S 表达式形式列表
;;
;; 返回值
;; ------
;; pair 或 #f
;; 找到的 define-library S 表达式；未找到时返回 #f。

(import (scheme base) (liii check) (liii goldcheck type-hint parse))

(check-set-mode! 'report-failed)

;; 1. 查找存在的 define-library
(let* ((forms '((import (scheme base))
                (define-library (foo bar) (export baz))
                (define x 10))
       ) ;forms
       (found (find-define-library forms))
      ) ;
  (check-true (pair? found))
  (check (car found) => 'define-library)
) ;let*

;; 2. 不存在时返回 #f
(check (find-define-library '((define a 1) (define b 2))) => #f)
(check (find-define-library '()) => #f)

(check-report)
