(import (liii check)
  (liii pi code core extensions)
) ;import

(check-set-mode! 'report-failed)

;; 1. 默认创建 ExtensionAPI，自动初始化关联的 Registry
(let ((api (make-extension-api)))
  (check (extension-api? api) => #t)
  (check (extension-registry? (extension-api-registry api)) => #t)
) ;let

;; 2. 指定已有的 Registry 创建 ExtensionAPI
(let* ((reg (make-extension-registry))
       (api (make-extension-api reg)))
  (check (extension-api? api) => #t)
  (check (eq? (extension-api-registry api) reg) => #t)
) ;let*

;; 3. 谓词对非 ExtensionAPI 对象返回 #f
(check (extension-api? "not-an-api") => #f)
(check (extension-api? '()) => #f)
(check (extension-api? 123) => #f)

;; 4. 传入非法的 Registry 参数抛出 type-error
(check-catch 'type-error (make-extension-api "invalid-registry"))
(check-catch 'type-error (make-extension-api 123))

(check-report)
