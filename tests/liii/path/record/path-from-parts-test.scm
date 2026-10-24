(import (liii check) (liii path) (liii os))

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
  (check (path->string (path-from-parts #("/" "tmp" "demo.txt")))
    =>
    "/tmp/demo.txt"
  ) ;check
  (check (path-parts (path-from-parts #("/" "tmp" "demo.txt")))
    =>
    #("/" "tmp" "demo.txt")
  ) ;check
) ;when

(when (os-windows?)
  ;; drive-absolute anchor 首段 "C:\"
  (check (path->string (path-from-parts #("C:\\" "tmp" "demo.txt")))
    =>
    "C:\\tmp\\demo.txt"
  ) ;check
  ;; drive-relative anchor 首段 "C:"(对齐 PureWindowsPath(*('C:','tmp')) => C:tmp)
  (check (path->string (path-from-parts #("C:" "tmp" "demo.txt")))
    =>
    "C:tmp\\demo.txt"
  ) ;check
  ;; UNC share anchor 首段
  (check (path->string (path-from-parts #("\\\\srv\\sh\\" "a" "b")))
    =>
    "\\\\srv\\sh\\a\\b"
  ) ;check
  ;; "\\" head: 视为当前盘根路径
  (check (path->string (path-from-parts #("\\" "tmp" "demo.txt")))
    =>
    "\\tmp\\demo.txt"
  ) ;check
  ;; round-trip:path-parts 输出可被 path-from-parts 重建
  (check (path-parts (path-from-parts (path-parts (path "C:\\tmp\\demo.txt"))))
    =>
    #("C:\\" "tmp" "demo.txt")
  ) ;check
) ;when

(check-report)
