(import (liii check)
  (liii path)
  (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-parent
;; 获取父路径。
;;
;; 语法
;; ----
;; (path-parent path-value) -> path-value
;;
;; 参数
;; ----
;; path-value : path-value
;; 路径值。
;;
;; 返回值
;; ----
;; path-value
;; 返回父目录的路径值。
;;
;; 描述
;; ----
;; path-parent 是 rich-path 中 :parent 的函数式版本。

(let ((sep (string (os-sep))))
  ;; path-parent 测试
  (check (path->string (path-parent (path "tmp/demo.txt"))
         ) ;path->string
    =>
    (string-append "tmp" sep)
  ) ;check
  (check (path->string (path-parent (path "tmp"))
         ) ;path->string
    =>
    "."
  ) ;check
  (check (path->string (path-parent (path "")))
    =>
    "."
  ) ;check

  (when (not (os-windows?))
    (check (path->string (path-parent (path-root)))
      =>
      "/"
    ) ;check
    (check (path->string (path-parent (path "/tmp/"))
           ) ;path->string
      =>
      "/"
    ) ;check
    (check (path->string (path-parent (path "/tmp/demo.txt"))
           ) ;path->string
      =>
      "/tmp/"
    ) ;check
    (check (path->string (path-parent (path-parent (path "/tmp/demo.txt"))
                         ) ;path-parent
           ) ;path->string
      =>
      "/"
    ) ;check
  ) ;when

  (when (os-windows?)
    (check (path->string (path-parent (path "C:\\Users"))
           ) ;path->string
      =>
      "C:\\"
    ) ;check
    (check (path->string (path-parent (path "a\\b"))
           ) ;path->string
      =>
      "a\\"
    ) ;check
  ) ;when
) ;let

(check-report)
