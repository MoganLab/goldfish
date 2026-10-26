(import (liii check)
  (liii pi code core extensions)
) ;import

(check-set-mode! 'report-failed)

;; 1. 默认创建 ExtensionContext
(let ((ctx (make-extension-context)))
  (check (extension-context? ctx) => #t)
  (check (extension-context-session-id ctx) => #f)
  (check (extension-context-cwd ctx) => ".")
  (check (extension-context-model ctx) => #f)
  (check (extension-registry? (extension-context-registry ctx)) => #t)
  (check (extension-context-extra ctx) => '())
) ;let

;; 2. 指定参数创建 ExtensionContext
(let* ((reg (make-extension-registry))
       (ctx (make-extension-context
              :session-id "sess-001"
              :cwd "/tmp/test-project"
              :model "kimi-k2"
              :registry reg
              :extra '((dry-run . #t)))))
  (check (extension-context? ctx) => #t)
  (check (extension-context-session-id ctx) => "sess-001")
  (check (extension-context-cwd ctx) => "/tmp/test-project")
  (check (extension-context-model ctx) => "kimi-k2")
  (check (eq? (extension-context-registry ctx) reg) => #t)
  (check (cdr (assq 'dry-run (extension-context-extra ctx))) => #t)
) ;let*

;; 3. 谓词对非 ExtensionContext 对象返回 #f
(check (extension-context? #f) => #f)
(check (extension-context? "context") => #f)
(check (extension-context? '(1 2 3)) => #f)

;; 4. 传入非法类型测试
(check-catch 'type-error (make-extension-context :session-id 12345))
(check-catch 'type-error (make-extension-context :cwd 999))
(check-catch 'type-error (make-extension-context :registry "bad-reg"))

(check-report)
