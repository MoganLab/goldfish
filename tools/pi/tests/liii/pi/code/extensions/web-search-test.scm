(import (liii check)
  (liii pi code extension)
  (liii pi code tools)
) ;import

(check-set-mode! 'report-failed)

(define (get-web-search-path)
  (cond
    ((file-exists? "tools/pi/extensions/web-search.scm")
     "tools/pi/extensions/web-search.scm")
    ((file-exists? "extensions/web-search.scm")
     "extensions/web-search.scm")
    (else "../../extensions/web-search.scm")
  ) ;cond
) ;define

;; 1. 加载 web-search.scm 扩展
(let* ((api (make-extension-api))
       (res (pi-load-extension (get-web-search-path) api)))
  (check (pair? res) => #t)
  (check (eq? (cdr (assq 'status res)) 'ok) => #t)

  ;; 2. 验证注册的 web_search 工具
  (let ((tool (find-extension-tool api "web_search")))
    (check (extension-tool-def? tool) => #t)
    (check (extension-tool-def-name tool) => "web_search")
    (check (extension-tool-def-description tool)
      => "Search the web using Gemini Google Search grounding."
    ) ;check
    ;; schema 是 JSON Schema alist，query 必填
    (let* ((schema (extension-tool-def-parameters tool))
           (required (cdr (assoc "required" schema))))
      (check (vector->list required) => '("query")))
    ;; execute 可调用（参数校验路径：非法参数报 type-error）
    (check (procedure? (extension-tool-def-execute tool)) => #t)
    (check-catch 'type-error ((extension-tool-def-execute tool) '()))
    (check (extension-tool-def-snippet tool)
      => "Search the web for up-to-date information"
    ) ;check
    (check (extension-tool-def-guidelines tool)
      => '("Use web_search when you need real-time information or external references.")
    ) ;check
    ;; 默认启用由扩展声明（:default-active? #t），核心默认集不写死 web_search
    (check (extension-tool-def-default-active? tool) => #t)
  ) ;let

  ;; 2b. 注册表默认工具名：声明 default-active? 的工具并入默认集
  (check (extension-registry-default-tool-names api) => '("web_search"))

  ;; 3. 注册表内 get-tool-definition / execute-tool-call 可分发 web_search
  (with-extension-registry api
    (lambda ()
      (let ((def (get-tool-definition "web_search")))
        (check-true (pi-tool-definition? def))
        (check (pi-tool-definition-name def) => "web_search")
        ;; 参数校验失败经 execute-tool-call 返回错误结果而非抛异常
        (let ((res2 (execute-tool-call "web_search" '())))
          (check-true (pi-tool-result-is-error res2)))
      ) ;let
    ) ;lambda
  ) ;with-extension-registry

  ;; 4. 重复加载幂等（upsert 语义，不重复注册）
  (pi-load-extension (get-web-search-path) api)
  (check (length (extension-registry-tools-list api)) => 1)
) ;let*

(check-report)
