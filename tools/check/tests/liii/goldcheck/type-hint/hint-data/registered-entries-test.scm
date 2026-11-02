;; registered-entries
;; 提取查询函数内部 case 表达式登记的函数条目列表。
;;
;; 语法
;; ----
;; (registered-entries info qname)
;;
;; 参数
;; ----
;; info : library-info   库解析信息对象
;; qname : symbol        查询函数名符号
;;
;; 返回值
;; ------
;; list of entry
;; 登记的各条目元组 ((name var line) ...) 列表。

(import (scheme base) (liii check) (liii goldcheck type-hint hint-data))

(check-set-mode! 'report-failed)

;; 1. 从含有 case 分支的查询函数提取条目
(let* ((expr
         '(define (hint-foo name) (case name ((bar) hint-bar) (else #f)))
       ) ;expr
       (d (list 'hint-foo 'func 1 10 expr))
       (info (list (cons 'defines (list d))))
       (entries (registered-entries info 'hint-foo))
      ) ;
  (check (length entries) => 1)
  (check (caar entries) => 'bar)
  (check (cadar entries) => 'hint-bar)
) ;let*

(check-report)
