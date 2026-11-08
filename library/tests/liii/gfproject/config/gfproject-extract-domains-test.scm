;; gfproject-extract-domains
;; 从配置对象中提取 domains 段。
;;
;; 语法
;; ----
;; (gfproject-extract-domains config)
;;
;; 参数
;; ----
;; config : list
;; 配置对象（形如 (("key" . value) ...)）。
;;
;; 返回值
;; ----
;; list
;; domains 段的对象；配置不是对象、没有 domains 键或 domains 不是对象时返回空对象 (())。

(import (liii check) (liii gfproject config) (liii json))

(check-set-mode! 'report-failed)

;; 正常提取
(let* ((config (string->json "{\"domains\": {\"r7rs\": {\"root\": \"r7rs/\"}}}"))
       (domains (gfproject-extract-domains config))
      ) ;
  (check-true (json-object? domains))
  (check (json-ref domains "r7rs") => '(("root" . "r7rs/")))
) ;let*

;; 没有 domains 键
(check (gfproject-extract-domains (string->json "{\"a\": 1}")) => '(()))

;; 配置不是对象
(check (gfproject-extract-domains '()) => '(()))

;; domains 不是对象
(check (gfproject-extract-domains (string->json "{\"domains\": [1]}")) => '(()))

(check-report)
