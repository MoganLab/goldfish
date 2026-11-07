;; default-extension-registry
;; 获取进程级默认扩展注册表，首次访问时创建并执行已注册的初始化器。
;;
;; 语法
;; ----
;; (default-extension-registry)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; extension-registry
;; 默认注册表记录。

(import (scheme base)
  (liii check)
  (liii pi code core extensions registry base)
  (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (default-extension-registry)))
  (check-true (extension-registry? reg))
  (check (default-extension-registry) => reg)
) ;let

(check-report)
