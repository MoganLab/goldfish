(import (liii check)
  (liii pi session) (liii pi tools)
  (liii pi tools)
  (liii pi web-search)
) ;import

(check-set-mode! 'report-failed)

;; pi-web-search-schema
;; 获取 web_search 工具的 JSON Schema 定义，形状对齐原生 pi-web-search 扩展。
(let* ((schema (pi-web-search-schema))
       (props (cdr (assoc "properties" schema)))
       (query (cdr (assoc "query" props)))
       (urls (cdr (assoc "urls" props)))
      ) ;
  (check (cdr (assoc "type" schema)) => "object")
  (check (cdr (assoc "required" schema)) => #("query"))
  ;; query：必填 string
  (check (cdr (assoc "type" query)) => "string")
  (check (cdr (assoc "description" query))
    => "The search query or question to answer"
  ) ;check
  ;; urls：可选 array<string>，最多 20 项
  (check (cdr (assoc "type" urls)) => "array")
  (check (cdr (assoc "maxItems" urls)) => 20)
  (check (cdr (assoc "type" (cdr (assoc "items" urls)))) => "string")
  (check (cdr (assoc "description" urls))
    => "Additional URLs to analyze along with search (up to 20)"
  ) ;check
  ;; urls 不在 required 中
  (check-true (not (member "urls" (vector->list (cdr (assoc "required" schema))))))
) ;let*

;; 三层导入均可用：直接引用同一个函数
(check-true (procedure? pi-web-search-schema))

(check-report)
