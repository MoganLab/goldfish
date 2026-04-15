(import (liii check)
  (liii path)
  (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path->string
;; 将路径值转换为字符串。
;;
;; 语法
;; ----
;; (path->string path-value)
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

(check (path->string (path)) => ".")
(check (path->string (path "")) => ".")
(check (path->string (path-root))
  =>
  "/"
) ;check
(check (path->string (path-of-drive #\C))
  =>
  "C:\\"
) ;check

(when (not (os-windows?))
  (check (path->string (path "tmp/demo.txt"))
    =>
    "tmp/demo.txt"
  ) ;check
  (check (path->string (path (path "tmp/demo.txt"))
         ) ;path->string
    =>
    "tmp/demo.txt"
  ) ;check
  (check (path->string (path-copy (path "tmp/demo.txt"))
         ) ;path->string
    =>
    "tmp/demo.txt"
  ) ;check
) ;when

(when (os-windows?)
  (check (path->string (path "tmp/demo.txt"))
    =>
    "tmp\\demo.txt"
  ) ;check
  (check (path->string (path (path "tmp/demo.txt"))
         ) ;path->string
    =>
    "tmp\\demo.txt"
  ) ;check
  (check (path->string (path-copy (path "tmp/demo.txt"))
         ) ;path->string
    =>
    "tmp\\demo.txt"
  ) ;check
) ;when

(check-report)
