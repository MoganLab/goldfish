;; make-extension-registry
;; 创建空的扩展注册表记录。
;;
;; 语法
;; ----
;; (make-extension-registry)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; extension-registry
;; 新建的空注册表记录。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types core)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (check-true (extension-registry? reg))
  (check (extension-registry-tools reg) => '())
  (check (extension-registry-flags reg) => '())
  (check (extension-registry-commands reg) => '())
  (check (extension-registry-listeners reg) => '())
  (check (extension-registry-flows reg) => '())
  (check (extension-registry-validators reg) => '())
  (check (extension-registry-augmenters reg) => '())
  (check (extension-registry-prompt-injectors reg) => '())
) ;let*

(check-report)
