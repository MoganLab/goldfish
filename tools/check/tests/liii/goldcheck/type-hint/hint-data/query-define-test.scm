;; query-define
;; 按约定名称在库解析信息中查找对应查询函数的定义记录。
;;
;; 语法
;; ----
;; (query-define info qname)
;;
;; 参数
;; ----
;; info : library-info   库解析信息对象
;; qname : symbol        查询函数名符号
;;
;; 返回值
;; ------
;; def-record 或 #f
;; 找到的定义记录；未找到返回 #f。

(import (scheme base) (liii check) (liii goldcheck type-hint hint-data))

(check-set-mode! 'report-failed)

;; 1. 查找存在的查询函数定义
(let* ((d '(hint-foo func 1 10 (define (hint-foo name) #f)))
       (info (list (cons 'defines (list d))))
      ) ;
  (check (query-define info 'hint-foo) => d)
  (check (query-define info 'hint-bar) => #f)
) ;let*

(check-report)
