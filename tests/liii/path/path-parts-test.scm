(import (liii check)
  (liii path)
  (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-parts
;; 获取路径的各个部分。
;;
;; 语法
;; ----
;; (path-parts path-value)
;;
;; 参数
;; ----
;; path-value : path-value
;; 要查询的路径值。
;;
;; 返回值
;; ----
;; vector
;; 返回包含路径各部分的字符串向量。

(check (path-parts (path)) => #("."))
(check (path-parts (path-root))
  =>
  #("/")
) ;check
(check (path-parts (path-of-drive #\c))
  =>
  #()
) ;check

(when (not (os-windows?))
  (check (path-parts (path-from-parts #("/" "tmp" "demo.txt")
                     ) ;path-from-parts
         ) ;path-parts
    =>
    #("/" "tmp" "demo.txt")
  ) ;check
) ;when

(check-report)
