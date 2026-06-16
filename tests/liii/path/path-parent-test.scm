(import (liii check) (liii path) (liii os))

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
    =>
    (string-append "tmp" sep)
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
    ;; s7 的 port-filename / argv 在 Windows 上返回正斜杠绝对路径,
    ;; path-parent 必须同时识别 / 和 \,否则会把整串当单一 part 剥到只剩 "C:"
    (check (path->string (path-parent (path "C:/Users/foo/bar.scm")))
      =>
      "C:\\Users\\foo\\"
    ) ;check
    ;; string 输入分支同样需要规范化(path->string 对 string 直通返回原串)
    (check (path->string (path-parent "C:/Users/foo/bar.scm"))
      =>
      "C:\\Users\\foo\\"
    ) ;check
  ) ;when
) ;let

(check-report)
