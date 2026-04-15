(import (liii check)
  (liii path)
  (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-copy
;; 复制路径值。
;;
;; 语法
;; ----
;; (path-copy path-value)
;;
;; 参数
;; ----
;; path-value : path-value
;; 要复制的路径值。
;;
;; 返回值
;; ----
;; path-value
;; 返回一个新的路径值副本。

(when (not (os-windows?))
  (check (path->string (path-copy (path "tmp/demo.txt"))
         ) ;path->string
    =>
    "tmp/demo.txt"
  ) ;check
) ;when

(when (os-windows?)
  (check (path->string (path-copy (path "tmp/demo.txt"))
         ) ;path->string
    =>
    "tmp\\demo.txt"
  ) ;check
) ;when

(check-true (path=? (path "tmp/demo.txt")
              (path-copy (path "tmp/demo.txt"))
            ) ;path=?
) ;check-true

(check-report)
