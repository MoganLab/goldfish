;; get-json-set
;; JSON 对象 upsert：已有键替换、缺失键（含空对象）新增，不产生重复键。
;;
;; 语法
;; ----
;; (get-json-set obj key value)
;;
;; 参数
;; ----
;; obj : json-object?
;; 目标对象（空对象为 '()）。
;;
;; key : string?
;; 键。
;;
;; value : any?
;; JSON 值。
;;
;; 返回值
;; ----
;; json-object?
;; 更新后的对象。

(import (liii check) (liii json) (liii gfproject get store))

(check-set-mode! 'report-failed)

;; 空对象新增
(let ((obj (get-json-set '() "a" 1)))
  (check (json-ref obj "a") => 1)
  (check (length (json-keys obj)) => 1)
) ;let

;; 缺失键新增，保留其他键
(let ((obj (get-json-set (string->json "{\"a\":1}") "b" 2)))
  (check (json-ref obj "a") => 1)
  (check (json-ref obj "b") => 2)
) ;let

;; 已有键替换，不重复
(let ((obj (get-json-set (string->json "{\"a\":1,\"b\":2}") "a" 9)))
  (check (json-ref obj "a") => 9)
  (check (length (json-keys obj)) => 2)
) ;let

;; 嵌套 upsert（depends 场景）
(let* ((config (string->json "{\"depends\":{\"x@y\":{\"url\":\"u\"}}}"))
       (depends (get-json-sub config "depends"))
       (config (get-json-set config "depends"
                 (get-json-set depends "x@y" (string->json "{\"url\":\"v\"}"))
               ) ;get-json-set
       ) ;config
      ) ;
  (check (json-ref-string (json-ref (json-ref config "depends") "x@y") "url" "") => "v")
  (check (length (json-keys (json-ref config "depends"))) => 1)
) ;let*

(check-report)
