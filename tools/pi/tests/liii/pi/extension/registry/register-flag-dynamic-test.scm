(import (liii check)
  (liii pi extension)
) ;import

(check-set-mode! 'report-failed)

;; 1. make-extension-flag-def 增强字段与访问器
(let ((f (make-extension-flag-def
           "--custom-opt"
           "Custom option with argument"
           :key 'custom-key
           :aliases '("-c" "--custom")
           :takes-arg? #t
           :default "default-val"
           :validator (lambda (v) (string? v)))))
  (check (extension-flag-def? f) => #t)
  (check (extension-flag-def-flag f) => "--custom-opt")
  (check (extension-flag-def-key f) => 'custom-key)
  (check (extension-flag-def-aliases f) => '("-c" "--custom"))
  (check (extension-flag-def-takes-arg? f) => #t)
  (check (extension-flag-def-default f) => "default-val")
  (check (procedure? (extension-flag-def-validator f)) => #t)
) ;let

;; 2. 自动推断 key：从 flag 字符串推导 symbol
(let ((f1 (make-extension-flag-def "--my-cool-flag" "desc"))
      (f2 (make-extension-flag-def "-v" "verbose")))
  (check (extension-flag-def-key f1) => 'my-cool-flag)
  (check (extension-flag-def-key f2) => 'v)
  (check (extension-flag-def-takes-arg? f1) => #f)
) ;let

;; 3. 非 boolean 类型自动推断 takes-arg? 为 #t
(let ((f (make-extension-flag-def "--count" "Count" :type 'number :default 1)))
  (check (extension-flag-def-takes-arg? f) => #t)
  (check (extension-flag-def-key f) => 'count)
  (check (extension-flag-def-default f) => 1)
) ;let

;; 4. 注册与通过 alias / flag 查询
(let* ((api (make-extension-api))
       (f (make-extension-flag-def "--foo" "Foo" :key 'foo :aliases '("-f"))))
  (register-flag! api f)
  (check (extension-flag-def? (find-extension-flag api "--foo")) => #t)
  (check (extension-flag-def? (find-extension-flag api "-f")) => #t)
) ;let*

(check-report)
