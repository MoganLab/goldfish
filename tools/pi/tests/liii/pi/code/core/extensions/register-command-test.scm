(import (liii check)
  (liii pi code core extensions)
) ;import

(check-set-mode! 'report-failed)

;; 1. make-extension-command-def 构造与访问器
(let ((cmd (make-extension-command-def
             "tdd"
             "Run TDD workflow"
             (lambda (args ctx) "tdd-executed")
             :params '((mode . "strict")))))
  (check (extension-command-def? cmd) => #t)
  (check (extension-command-def-name cmd) => "tdd")
  (check (extension-command-def-description cmd) => "Run TDD workflow")
  (check (procedure? (extension-command-def-handler cmd)) => #t)
  (check (pair? (extension-command-def-params cmd)) => #t)
) ;let

;; 2. register-command! 注册与查询
(let* ((api (make-extension-api))
       (cmd (make-extension-command-def
              "review"
              "Run code review"
              (lambda (args ctx) "reviewed"))))
  (register-command! api cmd)
  (let ((found (find-extension-command api "review")))
    (check (extension-command-def? found) => #t)
    (check (extension-command-def-name found) => "review")
    (check ((extension-command-def-handler found) '() #f) => "reviewed"))
) ;let*

;; 3. extension-register-command! 别名与 registry 支持
(let* ((reg (make-extension-registry))
       (cmd (make-extension-command-def
              "status"
              "Show status"
              (lambda (args ctx) "ok"))))
  (extension-register-command! reg cmd)
  (let ((found (find-extension-command reg "status")))
    (check (extension-command-def? found) => #t)
    (check (extension-command-def-name found) => "status"))
) ;let*

;; 4. 校验错误处理
(check-catch 'type-error
  (make-extension-command-def "" "desc" (lambda (a c) #t)))
(check-catch 'type-error
  (make-extension-command-def "name" 123 (lambda (a c) #t)))
(check-catch 'type-error
  (make-extension-command-def "name" "desc" "not-a-handler"))
(check-catch 'type-error
  (register-command! "bad-target" (make-extension-command-def "n" "d" (lambda (a c) #t))))

(check-report)
