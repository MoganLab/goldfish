(import (liii check)
        (liii path)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-from-parts
;; 从路径各部分构造路径值。
;;
;; 语法
;; ----
;; (path-from-parts vector-of-parts)
;;
;; 参数
;; ----
;; vector-of-parts : vector?
;; 包含路径各部分的字符串向量。
;;
;; 返回值
;; ----
;; path-value
;; 返回组合后的路径值。
;;
;; 示例
;; ----
;; (path->string (path-from-parts #("/" "tmp" "demo.txt"))) => "/tmp/demo.txt"

(when (not (os-windows?))
  (check (path->string (path-from-parts #("/" "tmp" "demo.txt"))) => "/tmp/demo.txt")
  (check (path-parts (path-from-parts #("/" "tmp" "demo.txt"))) => #("/" "tmp" "demo.txt"))
) ;when

(when (os-windows?)
  (check (path->string (path-from-parts #("C:" "tmp" "demo.txt"))) => "C:\\tmp\\demo.txt")
) ;when

(check-report)
