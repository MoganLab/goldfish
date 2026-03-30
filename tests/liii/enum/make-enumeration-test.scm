(import (liii check)
        (liii enum)
) ;import

(check-set-mode! 'report-failed)

;; make-enumeration
;; 创建包含所有名称的 enum-set。
;;
;; 语法
;; ----
;; (make-enumeration symbol-list)
;;
;; 参数
;; ----
;; symbol-list : list?
;; 符号名称列表。
;;
;; 返回值
;; ----
;; enum-set
;; 包含所有名称对应 enum 的集合。
;;
;; 注意
;; ----
;; R6RS 兼容接口，集合中的 value 与 name 相同。
;;
;; 示例
;; ----
;; (enum-set-every? (lambda (e) (eqv? (enum-name e) (enum-value e))) (make-enumeration '(red yellow green))) => #t
;;
;; 错误处理
;; ----
;; 无。

(let* ((ds '(red yellow green))
       (us-traffic-light (make-enumeration ds)))
  (check (enum-set-every? (lambda (e) (eqv? (enum-name e) (enum-value e)))
                          us-traffic-light)
         =>
         #t
  ) ;check
) ;let*

(check-report)
