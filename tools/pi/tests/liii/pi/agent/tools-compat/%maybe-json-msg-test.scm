;; %maybe-json-msg
;; 将 JSON 对象转换为 pi-message record，其余输入原样返回。
;;
;; 语法
;; ----
;; (%maybe-json-msg x)
;;
;; 参数
;; ----
;; x : any?
;; JSON 对象、alist 或 pi-message record。
;;
;; 返回值
;; ----
;; any?
;; 转换后的 pi-message record 或原值。

(import (liii check) (liii pi ai message) (liii pi agent tools-compat))

(check-set-mode! 'report-failed)

(check-true (pi-user-message? (%maybe-json-msg '(("role" . "user") ("content" . "hi")))))
(check (%maybe-json-msg '((role . "user"))) => '((role . "user")))

(check-report)
