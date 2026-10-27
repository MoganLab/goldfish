(import (liii check)
  (liii pi code core tools)
  (liii pi ai tools record)
  (liii pi code core extensions)
  (liii pi ai types)
) ;import

(check-set-mode! 'report-failed)

;; 1. 默认内置基础工具始终可获取（web_search 已剥离为扩展，不在内置名单）
(let ((names '("read" "bash" "edit" "write" "ls" "find" "grep")))
  (for-each
    (lambda (name)
      (let ((def (get-tool-definition name)))
        (check-true (pi-tool-definition? def))
        (check (pi-tool-definition-name def) => name)))
    names))

;; 2. 解耦验证：在未注册扩展工具时，原硬编码的扩展工具不再默认返回定义
(check (get-tool-definition "web_search") => #f)
(check (get-tool-definition "tdd_status") => #f)
(check (get-tool-definition "tdd_advance") => #f)
(check (get-tool-definition "tdd_reset") => #f)
(check (get-tool-definition "blueprint_update") => #f)
(check (get-tool-definition "blueprint_status") => #f)
(check (get-tool-definition "blueprint_next") => #f)
(check (get-tool-definition "codebase_map") => #f)
(check (get-tool-definition "code_tour") => #f)
(check (get-tool-definition "unknown_custom_tool") => #f)

;; 3. 动态扩展注册表接驳：注册自定义工具后可正常获取
(let* ((api (make-extension-api))
       (tool (make-extension-tool-def
               "calculator"
               "Do math calculations"
               '((type . "object"))
               (lambda (args) 42)
               :snippet "calc(expr)"
               :guidelines '("Use for math"))))
  (register-tool! api tool)
  (with-extension-registry api
    (lambda ()
      (let ((def (get-tool-definition "calculator")))
        (check-true (pi-tool-definition? def))
        (check (pi-tool-definition-name def) => "calculator")
        (check (pi-tool-definition-description def) => "Do math calculations")
        (check (pi-tool-definition-parameters def) => '((type . "object")))
        (check (procedure? (pi-tool-definition-execute def)) => #t)
        (check (pi-tool-definition-snippet def) => "calc(expr)")
        (check (pi-tool-definition-guidelines def) => '("Use for math")))))
  ;; 作用域退出后不再存在
  (check (get-tool-definition "calculator") => #f))

;; 4. 注册 alist 格式工具定义
(let ((api (make-extension-api)))
  (register-tool! api
    (list (cons 'name "alist_tool")
          (cons 'description "Alist defined tool")
          (cons 'parameters '((type . "object")))
          (cons 'execute (lambda (args) "alist"))
          (cons 'snippet "alist()")
          (cons 'guidelines '("alist guideline"))))
  (with-extension-registry api
    (lambda ()
      (let ((def (get-tool-definition "alist_tool")))
        (check-true (pi-tool-definition? def))
        (check (pi-tool-definition-name def) => "alist_tool")
        (check (pi-tool-definition-description def) => "Alist defined tool")
        (check (pi-tool-definition-snippet def) => "alist()")
        (check (pi-tool-definition-guidelines def) => '("alist guideline"))))))

;; 5. current-extension-registry 未设置时回退到 default-extension-registry：
;; TUI worker 会话（(liii go) 独立 s7 会话）没有 with-extension-registry 动态作用域，
;; 扩展工具必须能从默认注册表解析，否则工具 schema 会被静默过滤掉
(let* ((api (make-extension-api))
       (_ (register-tool! api
            (make-extension-tool-def
              "fallback_tool"
              "Resolved from default registry"
              '((type . "object"))
              (lambda (args) "fb"))))
       (_ (set-default-extension-registry! api))
       (def (get-tool-definition "fallback_tool")))
  (check-true (pi-tool-definition? def))
  (check (pi-tool-definition-name def) => "fallback_tool")
  (check (pi-tool-definition-description def) => "Resolved from default registry")
  ;; with-extension-registry 的优先级仍高于 default：作用域内以 current 为准
  (let ((other (make-extension-api)))
    (register-tool! other
      (make-extension-tool-def "fallback_tool" "Shadowed by current" '((type . "object"))
        (lambda (args) "other")))
    (with-extension-registry other
      (lambda ()
        (check (pi-tool-definition-description (get-tool-definition "fallback_tool"))
          => "Shadowed by current"))))
  ;; 恢复默认注册表，避免影响后续测试
  (set-default-extension-registry! #f))

;; read 定义的 record 与 alist 视图
(let ((def (get-tool-definition "read")))
  (check-true (pi-tool-definition? def))
  (check (pi-tool-definition-name def) => "read")
  (check (cdr (assoc 'name def)) => "read")
  (check (cdr (assoc 'snippet def)) => "Read file contents"))

;; bash 支持默认与自定义超时 schema
(let* ((def (get-tool-definition "bash"))
       (schema (pi-tool-definition-parameters def))
       (props (cdr (assoc "properties" schema)))
       (t-prop (cdr (assoc "timeout" props))))
  (check-true (pi-tool-definition? def))
  (check (cdr (assoc "description" t-prop))
    => "Timeout in seconds (optional, defaults to 600s)"))

(let* ((def (get-tool-definition "bash" 300))
       (schema (pi-tool-definition-parameters def))
       (props (cdr (assoc "properties" schema)))
       (t-prop (cdr (assoc "timeout" props))))
  (check (cdr (assoc "description" t-prop))
    => "Timeout in seconds (optional, defaults to 300s)"))

(let* ((def (get-tool-definition "bash" :timeout 150))
       (schema (pi-tool-definition-parameters def))
       (props (cdr (assoc "properties" schema)))
       (t-prop (cdr (assoc "timeout" props))))
  (check (cdr (assoc "description" t-prop))
    => "Timeout in seconds (optional, defaults to 150s)"))

(check-report)
