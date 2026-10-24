(import (liii check)
  (liii pi code extension)
) ;import

(check-set-mode! 'report-failed)

;; 1. make-extension-tool-def 构造与访问器
(let ((tool (make-extension-tool-def
              "sample_calc"
              "A sample calculation tool"
              '((type . "object")
                (properties . ((x . ((type . "number"))))))
              (lambda (args) (+ 1 1))
              :snippet "sample_calc(x=1)"
              :guidelines '("use for math"))))
  (check (extension-tool-def? tool) => #t)
  (check (extension-tool-def-name tool) => "sample_calc")
  (check (extension-tool-def-description tool) => "A sample calculation tool")
  (check (pair? (extension-tool-def-parameters tool)) => #t)
  (check (procedure? (extension-tool-def-execute tool)) => #t)
  (check (extension-tool-def-snippet tool) => "sample_calc(x=1)")
  (check (extension-tool-def-guidelines tool) => '("use for math"))
  ;; 未声明时缺省 #f（不默认激活）
  (check (extension-tool-def-default-active? tool) => #f)
) ;let

;; 1b. :default-active? 声明默认激活
(let ((tool (make-extension-tool-def
              "always_on"
              "Tool activated by default"
              '((type . "object"))
              (lambda (args) "ok")
              :default-active? #t)))
  (check (extension-tool-def-default-active? tool) => #t))

;; 1c. extension-registry-default-tool-names：只收 default-active? 的工具，按注册顺序
(let* ((api (make-extension-api))
       (t1 (make-extension-tool-def "d1" "d1" '((type . "object")) (lambda (a) 1)
              :default-active? #t))
       (t2 (make-extension-tool-def "opt2" "opt2" '((type . "object")) (lambda (a) 2)))
       (t3 (make-extension-tool-def "d3" "d3" '((type . "object")) (lambda (a) 3)
              :default-active? #t)))
  (register-tool! api t1)
  (register-tool! api t2)
  (register-tool! api t3)
  (check (extension-registry-default-tool-names api) => '("d1" "d3")))

;; 1d. alist 注册同样支持 default-active? 字段
(let* ((api (make-extension-api))
       (tool-alist (list (cons 'name "alist_dflt")
                         (cons 'description "alist default tool")
                         (cons 'parameters '((type . "object")))
                         (cons 'execute (lambda (args) 42))
                         (cons 'default-active? #t))))
  (register-tool! api tool-alist)
  (check (extension-registry-default-tool-names api) => '("alist_dflt")))

;; 2. register-tool! 注册 ExtensionToolDef
(let* ((api (make-extension-api))
       (tool (make-extension-tool-def
               "my_tool"
               "My custom extension tool"
               '((type . "object"))
               (lambda (args) "hello"))))
  (register-tool! api tool)
  (let ((found (find-extension-tool api "my_tool")))
    (check (extension-tool-def? found) => #t)
    (check (extension-tool-def-name found) => "my_tool")
    (check ((extension-tool-def-execute found) '()) => "hello"))
) ;let*

;; 3. register-tool! 支持通过 alist 格式注册
(let* ((api (make-extension-api))
       (tool-alist (list (cons 'name "alist_tool")
                         (cons 'description "Tool from alist")
                         (cons 'parameters '((type . "object")))
                         (cons 'execute (lambda (args) 42)))))
  (register-tool! api tool-alist)
  (let ((found (find-extension-tool api "alist_tool")))
    (check (extension-tool-def? found) => #t)
    (check (extension-tool-def-name found) => "alist_tool")
    (check ((extension-tool-def-execute found) '()) => 42))
) ;let*

;; 4. extension-register-tool! 别名与通过 registry 注册
(let* ((reg (make-extension-registry))
       (tool (make-extension-tool-def
               "reg_tool"
               "Tool in registry"
               '()
               (lambda (args) #t))))
  (extension-register-tool! reg tool)
  (let ((found (find-extension-tool reg "reg_tool")))
    (check (extension-tool-def? found) => #t)
    (check (extension-tool-def-name found) => "reg_tool"))
) ;let*

;; 5. 校验错误处理：字段不合法或必填项缺失抛出 type-error
(check-catch 'type-error
  (make-extension-tool-def "" "desc" '() (lambda (x) x)))
(check-catch 'type-error
  (make-extension-tool-def 123 "desc" '() (lambda (x) x)))
(check-catch 'type-error
  (make-extension-tool-def "t" 456 '() (lambda (x) x)))
(check-catch 'type-error
  (make-extension-tool-def "t" "desc" '() "not-a-procedure"))
(check-catch 'type-error
  (register-tool! "invalid-api" (make-extension-tool-def "t" "d" '() (lambda (x) x))))

(check-report)
