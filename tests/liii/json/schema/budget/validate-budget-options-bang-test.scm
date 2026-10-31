(import (liii check) (liii json schema budget))

(check-set-mode! 'report-failed)

;; validate-budget-options!
;; 校验资源预算相关的选项取值。
;;
;; 语法
;; ----
;; (validate-budget-options! options)
;;
;; 参数
;; ----
;; options : alist
;; 选项表，包含 max-depth、max-steps 与 max-references。
;;
;; 返回值
;; ----
;; unspecified
;; 校验通过时不返回有效值。
;;
;; 注意
;; ----
;; 三个预算选项都必须是 #f 或正整数，缺省时按 #f 处理。
;;
;; 示例
;; ----
;; (validate-budget-options! '((max-depth . 10)))
;;
;; 错误处理
;; ----
;; json-schema-options-error 当预算选项不是 #f 或正整数时。

;; 缺省或合法取值都通过
(validate-budget-options! '())
(validate-budget-options! '((max-depth . 10)
                            (max-steps . 100)
                            (max-references . 1000))
) ;validate-budget-options!
(validate-budget-options! '((max-depth . #f)
                            (max-steps . #f)
                            (max-references . #f))
) ;validate-budget-options!

;; 非正整数被拒绝
(check-catch 'json-schema-options-error
  (validate-budget-options! '((max-depth . 0)))
) ;check-catch
(check-catch 'json-schema-options-error
  (validate-budget-options! '((max-steps . -1)))
) ;check-catch
(check-catch 'json-schema-options-error
  (validate-budget-options! '((max-references . 0)))
) ;check-catch

;; 非整数被拒绝
(check-catch 'json-schema-options-error
  (validate-budget-options! '((max-depth . 1.5)))
) ;check-catch
(check-catch 'json-schema-options-error
  (validate-budget-options! '((max-steps . "10")))
) ;check-catch

(check-report)
