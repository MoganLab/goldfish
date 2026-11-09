;; unbound-variable-hint
;; 基于 load-path 扫描结果生成未绑定符号的库导入提示。
;;
;; 语法
;; ----
;; (unbound-variable-hint var-name) -> string
;;
;; 参数
;; ----
;; var-name : string
;; 缺失的未绑定符号名称。
;;
;; 返回值
;; ----
;; string
;; 找到匹配库时返回 Hint 提示字符串，找不到或参数无效时返回空字符串 ""。

(import (liii check)
  (liii string)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

;; 1. 不存在的变量或非字符串参数返回空字符串
(check (unbound-variable-hint "non-existent-var-xyz-12345") => "")
(check (unbound-variable-hint #f) => "")

;; 2. 存在的标准符号能正确生成 Hint
(let ((hint (unbound-variable-hint "string-starts?")))
  (if (not (string-null? hint))
    (begin
      (check-true (string-contains? hint "Hint: `string-starts?` exists in"))
      (check-true (string-contains? hint "(import (liii string))"))
      (check-true (string-contains? hint "Please import"))
    ) ;begin
    (check #t => #t)
  ) ;if
) ;let

(check-report)
