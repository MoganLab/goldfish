(import (liii check)
  (liii pi code extension)
) ;import

(check-set-mode! 'report-failed)

;; 1. make-extension-flag-def 构造与访问器
(let ((f (make-extension-flag-def
           "--my-flag"
           "Enable my custom flag"
           :handler (lambda (val) val)
           :type 'boolean
           :default #f)))
  (check (extension-flag-def? f) => #t)
  (check (extension-flag-def-flag f) => "--my-flag")
  (check (extension-flag-def-description f) => "Enable my custom flag")
  (check (procedure? (extension-flag-def-handler f)) => #t)
  (check (extension-flag-def-type f) => 'boolean)
  (check (extension-flag-def-default f) => #f)
) ;let

;; 2. register-flag! 注册与查询
(let* ((api (make-extension-api))
       (f (make-extension-flag-def "--verbose" "Verbose logging")))
  (register-flag! api f)
  (let ((found (find-extension-flag api "--verbose")))
    (check (extension-flag-def? found) => #t)
    (check (extension-flag-def-flag found) => "--verbose")
    (check (extension-flag-def-description found) => "Verbose logging"))
) ;let*

;; 3. extension-register-flag! 别名与 registry 支持
(let* ((reg (make-extension-registry))
       (f (make-extension-flag-def "--dry-run" "Dry run mode")))
  (extension-register-flag! reg f)
  (let ((found (find-extension-flag reg "--dry-run")))
    (check (extension-flag-def? found) => #t)
    (check (extension-flag-def-flag found) => "--dry-run"))
) ;let*

;; 4. 校验错误处理：flag 必须是非空字符串，description 必须是字符串
(check-catch 'type-error
  (make-extension-flag-def "" "desc"))
(check-catch 'type-error
  (make-extension-flag-def 'not-a-string "desc"))
(check-catch 'type-error
  (make-extension-flag-def "--f" 123))
(check-catch 'type-error
  (register-flag! 12345 (make-extension-flag-def "--f" "d")))

(check-report)
