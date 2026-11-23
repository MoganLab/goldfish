;; get-json-sub
;; 取 JSON 子对象；缺失、非对象或空配置时返回空对象。
;;
;; 语法
;; ----
;; (get-json-sub obj key)
;;
;; 参数
;; ----
;; obj : any?
;; JSON 对象。
;;
;; key : string?
;; 子对象键。
;;
;; 返回值
;; ----
;; json-object?
;; 子对象；无法取出时为 '()。

(import (liii check) (liii json) (liii gfproject get store))

(check-set-mode! 'report-failed)

(check (json-ref (get-json-sub (string->json "{\"a\":{\"x\":1}}") "a") "x") => 1)
(check (get-json-sub (string->json "{\"a\":1}") "a") => '(()))
(check (get-json-sub (string->json "{}") "a") => '(()))
(check (get-json-sub '() "a") => '(()))

(check-report)
