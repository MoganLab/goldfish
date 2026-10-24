(import (liii check)
  (liii pi code extension)
) ;import

(check-set-mode! 'report-failed)

;; 1. make-extension-flow-def 构造与访问器
(let ((flow (make-extension-flow-def
              'my-flow
              (lambda (parsed) (assoc 'my-flow parsed))
              (lambda (parsed cfg mode initial-msg) 0)
              :local? #t
              :priority 10)))
  (check (extension-flow-def? flow) => #t)
  (check (extension-flow-def-name flow) => 'my-flow)
  (check (procedure? (extension-flow-def-predicate flow)) => #t)
  (check (procedure? (extension-flow-def-handler flow)) => #t)
  (check (extension-flow-def-local? flow) => #t)
  (check (extension-flow-def-priority flow) => 10)
) ;let

;; 2. register-flow! 注册与查询
(let* ((reg (make-extension-registry))
       (flow1 (make-extension-flow-def
                'local-flow
                (lambda (p) #t)
                (lambda (p c m msg) 42)
                :local? #t))
       (flow2 (make-extension-flow-def
                'session-flow
                (lambda (p) #f)
                (lambda (p c m msg) 0)
                :local? #f)))
  (register-flow! reg flow1)
  (register-flow! reg flow2)
  (check (extension-flow-def? (find-extension-flow reg 'local-flow)) => #t)
  (check (extension-flow-def? (find-extension-flow reg 'session-flow)) => #t)
  (check (length (extension-registry-flows-list reg)) => 2)
) ;let*

;; 3. 校验器注册 register-validator!
(let* ((reg (make-extension-registry))
       (called #f)
       (v (lambda (parsed) (set! called #t) #t)))
  (register-validator! reg v)
  (check (length (extension-registry-validators-list reg)) => 1)
  (for-each (lambda (val-fn) (val-fn '())) (extension-registry-validators-list reg))
  (check called => #t)
) ;let*

(check-report)
