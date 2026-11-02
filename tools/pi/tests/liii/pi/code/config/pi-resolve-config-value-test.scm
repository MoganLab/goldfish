;; pi-resolve-config-value
;; 解析配置中的环境变量插值语法（$VAR / ${VAR}）。
;;
;; 语法
;; ----
;; (pi-resolve-config-value str [env]) -> string | #f
;;
;; 参数
;; ----
;; str : string
;; 待解析的配置值。
;; env : alist
;; 可选环境变量表。
;;
;; 返回值
;; ----
;; string | #f
;; 解析结果；变量缺失时返回 #f。

(import (liii check)
  (liii pi code config)
) ;import

(check-set-mode! 'report-failed)

(define TEST-ENV '(("GF_TEST_LEFT" . "left") ("GF_TEST_RIGHT" . "right")))

(check (pi-resolve-config-value "literal-key") => "literal-key")
(check (pi-resolve-config-value "$GF_TEST_LEFT" TEST-ENV) => "left")
(check (pi-resolve-config-value "${GF_TEST_LEFT}_$GF_TEST_RIGHT" TEST-ENV)
  =>
  "left_right"
) ;check
(check (pi-resolve-config-value "$GF_UNKNOWN_ENV_VAR" TEST-ENV) => #f)
(check-catch 'pi-config-error (pi-resolve-config-value "!echo 'not-allowed'"))
(check-catch 'type-error (pi-resolve-config-value 123))

(check-report)
