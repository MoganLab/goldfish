;; narrow-locals
;; 对局部变量类型映射表中的指定变量应用类型收窄。
;;
;; 语法
;; ----
;; (narrow-locals locals name pred)
;;
;; 参数
;; ----
;; locals : list  局部变量类型映射表 ((name . type) ...)
;; name : symbol  目标变量名
;; pred : symbol  收窄假设（如 'truthy 或具体谓词）
;;
;; 返回值
;; ------
;; list
;; 更新后的局部变量类型映射表。

(import (scheme base) (liii check) (liii goldcheck type-hint narrow))

(check-set-mode! 'report-failed)

;; 1. 收窄目标变量
(let* ((locals '((x or #f string?) (y . integer?)))
       (res (narrow-locals locals 'x 'truthy))
      ) ;
  (check (cdr (assq 'x res)) => 'string?)
  (check (cdr (assq 'y res)) => 'integer?)
) ;let*

;; 2. 目标变量不存在原样返回
(let* ((locals '((x . integer?))) (res (narrow-locals locals 'non-existent 'truthy)))
  (check res => locals)
) ;let*

(check-report)
