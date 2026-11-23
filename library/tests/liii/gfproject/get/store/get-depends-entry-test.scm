;; get-depends-entry
;; 从用户配置提取 depends 中的单个包声明。
;;
;; 语法
;; ----
;; (get-depends-entry config id)
;;
;; 参数
;; ----
;; config : any?
;; gfproject.json 解析后的配置对象。
;;
;; id : string?
;; 包标识 name@organization。
;;
;; 返回值
;; ----
;; json-object? or #f
;; 包声明；未注册时返回 #f。

(import (liii check) (liii json) (liii gfproject get store))

(check-set-mode! 'report-failed)

(define config
  (string->json "{\"depends\":{\"pi@liii\":{\"url\":\"https://example.com/r\"}}}")
) ;define

(check (json-ref-string (get-depends-entry config "pi@liii") "url" "")
  => "https://example.com/r"
) ;check
(check-false (get-depends-entry config "nosuch@liii"))
(check-false (get-depends-entry (string->json "{}") "pi@liii"))
(check-false (get-depends-entry '() "pi@liii"))
(check-false (get-depends-entry (string->json "{\"depends\":1}") "pi@liii"))

(check-report)
