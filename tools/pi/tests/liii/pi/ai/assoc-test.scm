(import (liii check)
  (liii pi ai event)
  (liii pi ai message)
  (liii pi ai config record)
) ;import

(check-set-mode! 'report-failed)

;; assoc
;; 在 alist 或 pi 数据 record 中查找指定键对应的 entry。
;;
;; 语法
;; ----
;; (assoc key obj)
;;
;; 参数
;; ----
;; key : symbol | string
;; 要查找的字段名。
;; obj : alist | record
;; 普通 alist 或 pi event/message/config/tools 数据 record。
;;
;; 返回值
;; ----
;; pair | #f
;; 找到时返回原始键值 entry，未找到时返回 #f。
;;
;; 说明
;; ----
;; symbol 和 string 键可交叉匹配，is-error 与 isError 视为同一字段。
;; credential record 在混合列表中按 provider 返回 (provider . key)。
;;
;; 示例
;; ----
;; (assoc "role" (pi-user-message "hello")) => '("role" . "user")

(define test-cred (pi-credential "google" "secret" "api_key"))

(check (assoc "role" (pi-user-message "hello")) => '("role" . "user"))
(check (assoc 'provider test-cred) => '(provider . "google"))
(check (assoc "provider" test-cred) => '(provider . "google"))
(check (assoc 'missing test-cred) => #f)
(check (assoc 'is-error '((isError . #t))) => '(isError . #t))
(check (assoc "a" '((a . 1))) => '(a . 1))
(check (assoc "google" (list (cons "other" "other-key") test-cred)) => '("google" . "secret"))

(check-report)
