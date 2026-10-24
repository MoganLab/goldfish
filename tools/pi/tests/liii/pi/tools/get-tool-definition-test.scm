(import (liii check)
  (liii pi tools)
  (liii pi ai tools record)
  (liii pi extension)
) ;import

(check-set-mode! 'report-failed)

;; 1. 默认内置基础工具始终可获取
(let ((names '("read" "bash" "edit" "write" "ls" "find" "grep" "web_search")))
  (for-each
    (lambda (name)
      (let ((def (get-tool-definition name)))
        (check-true (pi-tool-definition? def))
        (check (pi-tool-definition-name def) => name)))
    names))

;; 2. 解耦验证：在未注册扩展工具时，原硬编码的扩展工具不再默认返回定义
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

(check-report)
