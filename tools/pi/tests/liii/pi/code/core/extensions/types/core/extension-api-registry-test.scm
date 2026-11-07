;; extension-api-registry
;; 读取扩展 API 绑定的注册表记录。
;;
;; 语法
;; ----
;; (extension-api-registry api)
;;
;; 参数
;; ----
;; api : extension-api
;; 扩展 API 记录。
;;
;; 返回值
;; ----
;; extension-registry
;; 绑定的注册表记录。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi code core extensions types core)
) ;import

(check-set-mode! 'report-failed)

(let* ((reg (make-extension-registry)) (api (make-extension-api reg)))
  (check (extension-api-registry api) => reg)
) ;let*

(check-report)
