(import (liii alist)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; alist-ref/default
;; 在关联列表中按键查找值，找不到时返回默认值。
;;
;; 语法
;; ----
;; (alist-ref/default alist key default)
;; (alist-ref/default alist key default =)
;;
;; 参数
;; ----
;; alist : list
;; 关联列表。
;;
;; key : any
;; 要查找的键。
;;
;; default : any
;; 键不存在时返回的默认值。
;;
;; = : procedure 可选
;; 自定义比较过程，默认使用eqv?。
;;
;; 返回值
;; ----
;; any
;; 返回匹配值；当键不存在时返回default。
;;
;; 注意
;; ----
;; 这个接口不会抛出`key-error`，而是直接返回默认值。
;;
;; 示例
;; ----
;; (alist-ref/default '((a . 1)) 'b 2) => 2
;;
;; 错误处理
;; ----
;; 无

(check (alist-ref/default '((a . 1)) 'b 2) => 2)

(check-report)
