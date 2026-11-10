;; gfproject-extract-tools
;; 从配置对象中提取 tools 段。
;;
;; 语法
;; ----
;; (gfproject-extract-tools config)
;;
;; 参数
;; ----
;; config : list
;; 配置对象（形如 (("key" . value) ...)）。
;;
;; 返回值
;; ----
;; list
;; tools 段的对象；配置不是对象、没有 tools 键或 tools 不是对象时返回空对象 (())。

(import (liii check) (liii gfproject config) (liii json))

(check-set-mode! 'report-failed)

;; 正常提取
(let* ((config (string->json "{\"tools\": {\"fmt\": {\"module\": \"goldfmt\"}}}"))
       (tools (gfproject-extract-tools config))
      ) ;
  (check-true (json-object? tools))
  (check (json-ref tools "fmt") => '(("module" . "goldfmt")))
) ;let*

;; 没有 tools 键
(check (gfproject-extract-tools (string->json "{\"a\": 1}")) => '(()))

;; 配置不是对象
(check (gfproject-extract-tools '()) => '(()))

;; tools 不是对象
(check (gfproject-extract-tools (string->json "{\"tools\": [1]}")) => '(()))

(check-report)
