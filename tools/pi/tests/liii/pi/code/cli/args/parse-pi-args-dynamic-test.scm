;; parse-pi-args
;; 验证扩展注册的动态参数定义参与解析。
;;
;; 语法
;; ----
;; (parse-pi-args argv)
;;
;; 参数
;; ----
;; argv : list
;; 不包含可执行文件名称的字符串参数列表。
;;
;; 返回值
;; ----
;; alist
;; 解析后的参数字典。

(import (liii check) (liii pi code cli args) (liii pi code core extensions))

(check-set-mode! 'report-failed)

;; 1. 动态挂载 boolean flag
(let ((reg (make-extension-registry)))
  (register-flag! reg
    (make-extension-flag-def "--my-dry-run" "Dry run flag" :key 'dry-run)
  ) ;register-flag!
  (with-extension-registry reg
    (lambda ()
      ;; 默认值存在
      (check (pi-args-ref (parse-pi-args '()) 'dry-run) => #f)
      ;; 传入 flag 生效
      (check (pi-args-ref (parse-pi-args '("--my-dry-run")) 'dry-run) => #t)
    ) ;lambda
  ) ;with-extension-registry
) ;let

;; 2. 动态挂载带参 option（支持 alias 与 --key=val 形式）
(let ((reg (make-extension-registry)))
  (register-flag! reg
    (make-extension-flag-def "--target-arch"
      "Target architecture"
      :key        'target-arch
      :aliases    '("-a" "--arch")
      :takes-arg? #t
      :default    "x86_64"
    ) ;make-extension-flag-def
  ) ;register-flag!
  (with-extension-registry reg
    (lambda ()
      ;; 默认值
      (check (pi-args-ref (parse-pi-args '()) 'target-arch) => "x86_64")
      ;; 长选项空格传参
      (check (pi-args-ref (parse-pi-args '("--target-arch" "arm64")) 'target-arch)
        =>
        "arm64"
      ) ;check
      ;; 别名传参
      (check (pi-args-ref (parse-pi-args '("-a" "riscv64")) 'target-arch)
        =>
        "riscv64"
      ) ;check
      ;; 等号传参
      (check (pi-args-ref (parse-pi-args '("--target-arch=loongarch64")) 'target-arch)
        =>
        "loongarch64"
      ) ;check
      (check (pi-args-ref (parse-pi-args '("--arch=mips")) 'target-arch) => "mips")
    ) ;lambda
  ) ;with-extension-registry
) ;let

;; 3. 动态验证器 validator
(let ((reg (make-extension-registry)))
  (register-flag! reg (make-extension-flag-def "--opt-x" "Option X" :key 'opt-x))
  (register-flag! reg (make-extension-flag-def "--opt-y" "Option Y" :key 'opt-y))
  (register-validator! reg
    (lambda (parsed)
      (when (and (pi-args-ref parsed 'opt-x) (pi-args-ref parsed 'opt-y))
        (error 'pi-arg-error "--opt-x and --opt-y cannot be used together")
      ) ;when
    ) ;lambda
  ) ;register-validator!
  (with-extension-registry reg
    (lambda ()
      (check (pi-validate-review-flags (parse-pi-args '("--opt-x"))) => #t)
      (check-catch 'pi-arg-error
        (pi-validate-review-flags (parse-pi-args '("--opt-x" "--opt-y")))
      ) ;check-catch
    ) ;lambda
  ) ;with-extension-registry
) ;let

(check-report)
