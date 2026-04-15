(import (liii check)
  (liii path)
  (liii os)
  (liii base)
) ;import

(check-set-mode! 'report-failed)

;; path-from-env
;; 从环境变量构造路径值。
;;
;; 语法
;; ----
;; (path-from-env name)
;;
;; 参数
;; ----
;; name : string?
;; 环境变量名。
;;
;; 返回值
;; ----
;; path-value
;; 返回环境变量值对应的路径。
;;
;; 示例
;; ----
;; (path->string (path-from-env "HOME")) => (getenv "HOME")

(when (not (os-windows?))
  (check (path->string (path-from-env "HOME"))
    =>
    (getenv "HOME")
  ) ;check
) ;when

(when (os-windows?)
  (check (path->string (path-from-env "USERPROFILE")
         ) ;path->string
    =>
    (getenv "USERPROFILE")
  ) ;check
) ;when

(check-report)
