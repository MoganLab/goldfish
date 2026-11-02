;; pi-resolve-config-value
;; 解析配置中的环境变量插值语法（$VAR / ${VAR}）。
;;
;; 语法
;; ----
;; (pi-resolve-config-value str . custom-env)
;;
;; 参数
;; ----
;; str : string
;; 待解析的配置值。
;; custom-env : alist
;; 可选环境变量表。
;;
;; 返回值
;; ----
;; string | #f
;; 插值后的字符串；引用的变量不存在时返回 #f。

(import (liii check) (liii pi code config))

(check-set-mode! 'report-failed)

;; pi-resolve-config-value
;; 解析配置中的环境变量插值语法 ($VAR / ${VAR})。
(check (pi-resolve-config-value "literal-key") => "literal-key")
(let ((test-env '(("TEST_LEFT" . "left") ("TEST_RIGHT" . "right"))))
  (check (pi-resolve-config-value "$TEST_LEFT" test-env) => "left")
  (check (pi-resolve-config-value "${TEST_LEFT}_$TEST_RIGHT" test-env) => "left_right")
  (check (pi-resolve-config-value "$$TEST_LEFT" test-env) => "$TEST_LEFT")
  (check (pi-resolve-config-value "$!literal-$TEST_RIGHT" test-env) => "!literal-right")
  (check (pi-resolve-config-value "$UNKNOWN_ENV_VAR_123" test-env) => #f)
  (check (pi-resolve-config-value "${UNKNOWN_ENV_VAR_123}" test-env) => #f))

(check-catch 'pi-config-error (pi-resolve-config-value "!echo 'not-allowed'"))
(check-catch 'type-error (pi-resolve-config-value 123))

(check-report)
