;; %maybe-json-block
;; 将 JSON 对象或兼容块 alist 转换为 pi-block record。
;;
;; 语法
;; ----
;; (%maybe-json-block x)
;;
;; 参数
;; ----
;; x : any?
;; JSON 对象、兼容 alist 或 pi-block record。
;;
;; 返回值
;; ----
;; any?
;; 转换后的 pi-block record 或原值。

(import (liii check) (liii pi ai message) (liii pi agent tools-compat))

(check-set-mode! 'report-failed)

(check-true (pi-text-block? (%maybe-json-block '(("type" . "text") ("text" . "hi")))))
(check-true (pi-text-block? (%maybe-json-block '((type . "text") (text . "hi")))))
(check (%maybe-json-block "raw") => "raw")

(check-report)
