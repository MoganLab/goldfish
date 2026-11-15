;; make-pi-provider-entry
;; 构造 <pi-provider-entry> 配置记录。
;;
;; 语法
;; ----
;; (make-pi-provider-entry . kwargs) -> pi-provider-entry?

;;
;; 参数
;; ----
;; entry : pi-provider-entry?
;;
;; 返回值
;; ----
;; any

(import (scheme base) (liii check) (liii pi code config user-providers))

(check-set-mode! 'report-failed)

(let ((e (make-pi-provider-entry "https://schema.example"
           "Kimi"
           "https://api.kimi.com/v1"
           "openai"
           "http://127.0.0.1:7890"
           '("k3.1-pro")
           '(("custom" . 1))
         ) ;make-pi-provider-entry
      ) ;e
     ) ;
  (check-true (pi-provider-entry? e))
  (check (pi-provider-entry-schema e) => "https://schema.example")
  (check (pi-provider-entry-name e) => "Kimi")
  (check (pi-provider-entry-base-url e) => "https://api.kimi.com/v1")
  (check (pi-provider-entry-api e) => "openai")
  (check (pi-provider-entry-http-proxy e) => "http://127.0.0.1:7890")
  (check (pi-provider-entry-models e) => '("k3.1-pro"))
  (check (pi-provider-entry-extras e) => '(("custom" . 1)))
) ;let

(let ((e (make-pi-provider-entry)))
  (check-true (pi-provider-entry? e))
  (check (pi-provider-entry-schema e) => #f)
  (check (pi-provider-entry-extras e) => '())
) ;let

(check-report)
