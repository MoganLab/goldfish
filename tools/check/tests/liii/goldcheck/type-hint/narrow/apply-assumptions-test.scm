;; apply-assumptions
;; 依次将收窄假设列表应用到局部变量类型映射表。
;;
;; 语法
;; ----
;; (apply-assumptions locals assumps)
;;
;; 参数
;; ----
;; locals : list   局部变量类型映射表
;; assumps : list  收窄假设列表 ((var . pred) ...)
;;
;; 返回值
;; ------
;; list
;; 批量收窄后的局部变量类型映射表。

(import (scheme base) (liii check) (liii goldcheck type-hint narrow))

(check-set-mode! 'report-failed)

;; 1. 批量应用收窄假设
(let* ((locals '((x or #f string?) (y or #f integer?)))
       (assumps '((x . truthy) (y . integer?)))
       (res (apply-assumptions locals assumps))
      ) ;
  (check (cdr (assq 'x res)) => 'string?)
  (check (cdr (assq 'y res)) => 'integer?)
) ;let*

;; 2. 空假设列表原样返回
(let* ((locals '((x or #f string?))) (res (apply-assumptions locals '())))
  (check res => locals)
) ;let*

(check-report)
