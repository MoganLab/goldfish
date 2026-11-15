;; make-pi-model-entry
;; 构造 <pi-model-entry> 配置记录。
;;
;; 语法
;; ----
;; (make-pi-model-entry . kwargs) -> pi-model-entry?

;;
;; 参数
;; ----
;; entry : pi-model-entry?
;;
;; 返回值
;; ----
;; any

(import (scheme base) (liii check) (liii pi code config types))

(check-set-mode! 'report-failed)

(let ((e (make-pi-model-entry "k3.1-pro"
           "Kimi K3.1 Pro"
           "https://api.kimi.com/v1"
           "openai"
           #t
           1048576
           131072
           '(("cost" . 0))
         ) ;make-pi-model-entry
      ) ;e
     ) ;
  (check-true (pi-model-entry? e))
  (check (pi-model-entry-id e) => "k3.1-pro")
  (check (pi-model-entry-name e) => "Kimi K3.1 Pro")
  (check (pi-model-entry-base-url e) => "https://api.kimi.com/v1")
  (check (pi-model-entry-api e) => "openai")
  (check (pi-model-entry-reasoning e) => #t)
  (check (pi-model-entry-context-window e) => 1048576)
  (check (pi-model-entry-max-tokens e) => 131072)
  (check (pi-model-entry-extras e) => '(("cost" . 0)))
) ;let

(let ((e (make-pi-model-entry)))
  (check-true (pi-model-entry? e))
  (check (pi-model-entry-reasoning e) => 'unset)
  (check (pi-model-entry-extras e) => '())
) ;let

(check-report)
