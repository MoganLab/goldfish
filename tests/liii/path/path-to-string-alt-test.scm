(import (liii check)
        (liii path)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-to-string
;; 将路径值转换为字符串（path->string 的别名）。
;;
;; 语法
;; ----
;; (path-to-string path-value)
;;
;; 参数
;; ----
;; path-value : path-value
;; 要转换的路径值。
;;
;; 返回值
;; ----
;; string
;; 返回路径的字符串表示。

(when (not (os-windows?))
  (check (path-to-string (path "tmp/demo.txt")) => "tmp/demo.txt")
) ;when

(when (os-windows?)
  (check (path-to-string (path "tmp/demo.txt")) => "tmp\\demo.txt")
) ;when

(check-report)
