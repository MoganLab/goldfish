;; hint-var-defs
;; 从库解析信息中过滤提取所有值类型且名称以 hint 开头的变量定义。
;;
;; 语法
;; ----
;; (hint-var-defs info)
;;
;; 参数
;; ----
;; info : library-info   库解析信息对象
;;
;; 返回值
;; ------
;; list of def-record
;; 符合条件的变量定义记录列表。

(import (scheme base)
  (liii check)
  (liii goldcheck type-hint parse)
  (liii goldcheck type-hint hint-data)
) ;import

(check-set-mode! 'report-failed)

;; 1. 解析并提取 hint 变量
(let* ((info
         (list (cons 'lib-name '(hint foo))
           (cons 'line 1)
           (cons 'imports '())
           (cons 'exports '())
           (cons 'defines
             (list
               '(hint-foo value #f 1 (define hint-foo '((any? -> any?))))
               '(bar func 0 2 (define (bar) #t))
             ) ;list
           ) ;cons
         ) ;list
       ) ;info
       (defs (hint-var-defs info))
      ) ;
  (check (length defs) => 1)
  (check (def-name (car defs)) => 'hint-foo)
) ;let*

(check-report)
