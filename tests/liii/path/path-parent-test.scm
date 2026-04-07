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
  (check (path->string (path-parent (path "tmp/demo.txt")))
         => (string-append "tmp" sep)
  ) ;check
  (check (path->string (path-parent (path "tmp"))) => ".")
  (check (path->string (path-parent (path ""))) => ".")

  (when (not (os-windows?))
    (check (path->string (path-parent (path-root))) => "/")
    (check (path->string (path-parent (path "/tmp/"))) => "/")
    (check (path->string (path-parent (path "/tmp/demo.txt"))) => "/tmp/")
    (check (path->string (path-parent (path-parent (path "/tmp/demo.txt")))) => "/")
  ) ;when

  (when (os-windows?)
    (check (path->string (path-parent (path "C:\\Users"))) => "C:\\")
    (check (path->string (path-parent (path "a\\b"))) => "a\\")
  ) ;when
) ;let

(check-report)
