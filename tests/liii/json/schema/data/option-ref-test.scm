(import (liii check) (liii json schema data))

(check-set-mode! 'report-failed)

;; option-ref
;; 读取选项表中与给定键匹配的值。
;;
;; 语法
;; ----
;; (option-ref options key default)
;;
;; 参数
;; ----
;; options : alist
;; 选项表，元素为 (键 . 值) 形式。
;;
;; key : symbol
;; 待查找的选项名。
;;
;; default : any?
;; 选项不存在时返回的默认值。
;;
;; 返回值
;; ----
;; any?
;; 选项存在时返回对应的值，否则返回 default。
;;
;; 注意
;; ----
;; 选项值为 #f 时返回 #f 而不是 default。
;;
;; 示例
;; ----
;; (option-ref '((max-errors . 2)) 'max-errors #f) => 2
;;
;; 错误处理
;; ----
;; 无。

(define options '((formats . #f) (max-errors . 2)))

(check (option-ref options 'max-errors #f) => 2)
(check-false (option-ref options 'formats #t))
(check (option-ref options 'registry 'missing) => 'missing)
(check (option-ref '() 'max-errors 'missing) => 'missing)
(check-false (option-ref '((formats . #f)) 'formats #t))

(check-report)
