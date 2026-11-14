;; pi-tool-result->json
;; 将 <pi-tool-result> record 转换为符合 (liii json) 规范的 Scheme-JSON 对象。
;;
;; 语法
;; ----
;; (pi-tool-result->json res)
;;
;; 参数
;; ----
;; res : any?
;; <pi-tool-result> record 或 alist。
;;
;; 返回值
;; ----
;; any?
;; 可直接用于 json->string 或 json-ref 的 Scheme-JSON 对象。

(import (liii check)
  (liii json)
  (liii pi ai tools record)
  (liii pi ai message record)
) ;import

(check-set-mode! 'report-failed)

(check-true (procedure? pi-tool-result->json))

;; 1. 字符串构造的 tool-result（符号键归一为字符串键）
(let ((j (pi-tool-result->json (pi-tool-result "done" #f))))
  (check (json-ref j "isError") => #f)
  (check-true (vector? (json-ref j "content")))
  (check (json-ref (vector-ref (json-ref j "content") 0) "type") => "text")
  (check (json-ref (vector-ref (json-ref j "content") 0) "text") => "done")
  (check (json->string j)
    =>
    "{\"content\":[{\"type\":\"text\",\"text\":\"done\"}],\"isError\":false}"
  ) ;check
) ;let

;; 2. 包含 pi-block record 的 tool-result
(let ((j (pi-tool-result->json
           (pi-tool-result (list (pi-text-block "err detail")) #t)
         ) ;pi-tool-result->json
      ) ;j
     ) ;
  (check (json-ref j "isError") => #t)
  (check (json-ref (vector-ref (json-ref j "content") 0) "text") => "err detail")
  (check (json->string j)
    =>
    "{\"content\":[{\"type\":\"text\",\"text\":\"err detail\"}],\"isError\":true}"
  ) ;check
) ;let

(check-report)
