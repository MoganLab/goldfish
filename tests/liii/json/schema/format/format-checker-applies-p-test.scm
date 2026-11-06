;; format-checker-applies?
;; 判断某个格式是否参与断言。
;;
;; 语法
;; ----
;; (format-checker-applies? format options)
;;
;; 参数
;; ----
;; format : string | symbol
;; 格式名称。
;;
;; options : alist
;; 校验器选项。
;;
;; 返回值
;; ----
;; boolean
;; 该格式参与断言返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; formats 选项控制哪些格式参与断言：
;; #f 表示 format 只作为注解，
;; #t 表示断言所有已知格式，
;; 列表表示只断言列出的格式。
;;
;; 格式名不区分大小写。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format))

(check-set-mode! 'report-failed)

;; 返回值是 member 的结果（真值但不严格等于 #t），这里归一化为布尔。

(define (truthy? value)
  (not (eq? value #f))
) ;define

;; 未开启 formats 时 format 只作为注解。

(check-false (truthy? (format-checker-applies? "email" '())))
(check-false
  (truthy? (format-checker-applies? "email" '((formats . #f))))
) ;check-false

;; formats 为 #t 时断言所有已知格式。

(check-true
  (truthy? (format-checker-applies? "email" '((formats . #t))))
) ;check-true
(check-true
  (truthy? (format-checker-applies? "date" '((formats . #t))))
) ;check-true

;; formats 为列表时只断言列出的格式。

(check-true
  (truthy? (format-checker-applies? "email" '((formats email))))
) ;check-true
(check-false
  (truthy? (format-checker-applies? "date" '((formats email))))
) ;check-false

;; 格式名不区分大小写。

(check-true
  (truthy? (format-checker-applies? "email" '((formats email))))
) ;check-true
(check-true
  (truthy? (format-checker-applies? "EMAIL" '((formats email))))
) ;check-true
(check-true
  (truthy? (format-checker-applies? 'email '((formats email))))
) ;check-true
(check-false
  (truthy? (format-checker-applies? "email" '((formats date))))
) ;check-false

(check-report)
