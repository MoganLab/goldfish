(import (liii check)
  (liii pi code core extensions)
) ;import

(check-set-mode! 'report-failed)

;; 1. 初始状态为 #f
(check (current-extension-registry) => #f)

;; 2. set-current-extension-registry! 设置 registry
(let ((reg (make-extension-registry)))
  (set-current-extension-registry! reg)
  (check (eq? (current-extension-registry) reg) => #t)
  ;; 清理恢复
  (set-current-extension-registry! #f)
  (check (current-extension-registry) => #f))

;; 3. 支持传入 extension-api 自动解包 registry
(let* ((api (make-extension-api))
       (reg (extension-api-registry api)))
  (set-current-extension-registry! api)
  (check (eq? (current-extension-registry) reg) => #t)
  (set-current-extension-registry! #f))

;; 4. 支持传入 extension-context 自动解包 registry
(let* ((ctx (make-extension-context))
       (reg (extension-context-registry ctx)))
  (set-current-extension-registry! ctx)
  (check (eq? (current-extension-registry) reg) => #t)
  (set-current-extension-registry! #f))

;; 5. 传入非法类型报错 type-error
(check-catch 'type-error (set-current-extension-registry! "not-a-registry"))
(check-catch 'type-error (set-current-extension-registry! 12345))

;; 6. with-extension-registry 动态作用域
(let ((reg1 (make-extension-registry))
      (reg2 (make-extension-registry)))
  (set-current-extension-registry! reg1)
  (check (eq? (current-extension-registry) reg1) => #t)
  (with-extension-registry reg2
    (lambda ()
      (check (eq? (current-extension-registry) reg2) => #t)))
  ;; 恢复到 reg1
  (check (eq? (current-extension-registry) reg1) => #t)
  ;; 异常时也能恢复
  (catch #t
    (lambda ()
      (with-extension-registry reg2
        (lambda ()
          (check (eq? (current-extension-registry) reg2) => #t)
          (error "boom"))))
    (lambda (tag . info) #f))
  (check (eq? (current-extension-registry) reg1) => #t)
  (set-current-extension-registry! #f))

(check-report)
