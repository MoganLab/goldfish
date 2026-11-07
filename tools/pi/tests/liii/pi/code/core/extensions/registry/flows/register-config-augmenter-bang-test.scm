;; register-config-augmenter!
;; 向注册表追加配置增强过程。
;;
;; 语法
;; ----
;; (register-config-augmenter! target proc)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; proc : procedure
;; 配置增强过程。
;;
;; 返回值
;; ----
;; procedure
;; 注册的配置增强过程。

(import (scheme base)
        (liii check)
        (liii pi code core extensions registry flows)
        (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (register-config-augmenter! reg (lambda (cfg) cfg))
  (check (length (extension-registry-augmenters reg)) => 1)
  (check-catch 'type-error (register-config-augmenter! reg "not-a-procedure"))
) ;let*

(check-report)
