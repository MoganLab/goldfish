;; lib-defines
;; 从库解析信息 alist 中提取在 begin 块内定义的函数或变量记录列表。
;;
;; 语法
;; ----
;; (lib-defines info)
;;
;; 参数
;; ----
;; info : list  库解析数据结构 alist
;;
;; 返回值
;; ------
;; list of def
;; define 定义记录列表。

(import (scheme base) (liii check) (liii goldcheck type-hint parse))

(check-set-mode! 'report-failed)

;; 1. 获取定义记录列表
(let* ((info
         '((defines (f1 lambda 1 10 (define (f1 x) x))))
       ) ;info
       (defs (lib-defines info))
      ) ;
  (check-true (list? defs))
  (check (length defs) => 1)
) ;let*

(check-report)
