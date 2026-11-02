;; pi-resolve-config-value
;; 解析字符串中的环境变量模板与转义符。
;;
;; 语法
;; ----
;; (pi-resolve-config-value str)
;; (pi-resolve-config-value str custom-env)
;;
;; 参数
;; ----
;; str : string?
;; 待解析的模板字符串。
;;
;; custom-env : alist?
;; 可选，自定义环境变量关联表。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 变量解析与插值后的字符串；若引用了未定义的变量则返回 #f。

(import (liii check) (liii pi ai template))

(check-set-mode! 'report-failed)

(define test-env '(("GF_TEST_LEFT" . "left") ("GF_TEST_RIGHT" . "right")))

(check (pi-resolve-config-value "literal-key") => "literal-key")
(check (pi-resolve-config-value "$GF_TEST_LEFT" test-env) => "left")
(check (pi-resolve-config-value "${GF_TEST_LEFT}_$GF_TEST_RIGHT" test-env)
  =>
  "left_right"
) ;check
(check (pi-resolve-config-value "$GF_UNKNOWN_ENV_VAR" test-env) => #f)
(check-catch 'pi-config-error (pi-resolve-config-value "!echo 'not-allowed'"))
(check-catch 'type-error (pi-resolve-config-value 123))

(check-report)
