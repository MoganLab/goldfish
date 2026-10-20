(import (liii check)
  (liii pi args)
  (liii pi extension)
) ;import

(check-set-mode! 'report-failed)

;; 1. 动态挂载 boolean flag
(let ((reg (make-extension-registry)))
  (register-flag! reg (make-extension-flag-def "--my-dry-run" "Dry run flag" :key 'dry-run))
  (with-extension-registry reg
    (lambda ()
      ;; 默认值存在
      (check (pi-args-ref (parse-pi-args '()) 'dry-run) => #f)
      ;; 传入 flag 生效
      (check (pi-args-ref (parse-pi-args '("--my-dry-run")) 'dry-run) => #t)
    )
  )
)

;; 2. 动态挂载带参 option（支持 alias 与 --key=val 形式）
(let ((reg (make-extension-registry)))
  (register-flag! reg (make-extension-flag-def
                        "--target-arch"
                        "Target architecture"
                        :key 'target-arch
                        :aliases '("-a" "--arch")
                        :takes-arg? #t
                        :default "x86_64"))
  (with-extension-registry reg
    (lambda ()
      ;; 默认值
      (check (pi-args-ref (parse-pi-args '()) 'target-arch) => "x86_64")
      ;; 长选项空格传参
      (check (pi-args-ref (parse-pi-args '("--target-arch" "arm64")) 'target-arch) => "arm64")
      ;; 别名传参
      (check (pi-args-ref (parse-pi-args '("-a" "riscv64")) 'target-arch) => "riscv64")
      ;; 等号传参
      (check (pi-args-ref (parse-pi-args '("--target-arch=loongarch64")) 'target-arch) => "loongarch64")
      (check (pi-args-ref (parse-pi-args '("--arch=mips")) 'target-arch) => "mips")
    )
  )
)

;; 3. 动态验证器 validator
(let ((reg (make-extension-registry)))
  (register-flag! reg (make-extension-flag-def "--opt-x" "Option X" :key 'opt-x))
  (register-flag! reg (make-extension-flag-def "--opt-y" "Option Y" :key 'opt-y))
  (register-validator! reg
    (lambda (parsed)
      (when (and (pi-args-ref parsed 'opt-x) (pi-args-ref parsed 'opt-y))
        (error 'pi-arg-error "--opt-x and --opt-y cannot be used together")
      )
    )
  )
  (with-extension-registry reg
    (lambda ()
      (check (pi-validate-review-flags (parse-pi-args '("--opt-x"))) => #t)
      (check-catch 'pi-arg-error
        (pi-validate-review-flags (parse-pi-args '("--opt-x" "--opt-y")))
      )
    )
  )
)

(check-report)
