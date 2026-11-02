;; find-def
;; 在定义记录列表中按符号名称检索首个匹配的定义记录。
;;
;; 语法
;; ----
;; (find-def name ds)
;;
;; 参数
;; ----
;; name : symbol         目标定义名称符号
;; ds : list of record   定义记录列表
;;
;; 返回值
;; ------
;; def-record 或 #f
;; 匹配的定义记录；未检索到时返回 #f。

(import (scheme base) (liii check) (liii goldcheck type-hint hint-data))

(check-set-mode! 'report-failed)

;; 1. 成功查找到定义
(let* ((d1 '(foo func 0 1 (define (foo) #t)))
       (d2 '(bar func 0 2 (define (bar) #t)))
       (defs (list d1 d2))
      ) ;
  (check (find-def 'foo defs) => d1)
  (check (find-def 'bar defs) => d2)
  (check (find-def 'baz defs) => #f)
) ;let*

(check-report)
