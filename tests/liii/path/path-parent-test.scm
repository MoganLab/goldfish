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
  ;; path-parent 测试(对齐 pathlib.parent,不含末尾分隔符)
  (check (path->string (path-parent (path "tmp/demo.txt"))) => "tmp")
  (check (path->string (path-parent (path "tmp"))) => ".")
  (check (path->string (path-parent (path ""))) => ".")
  ;; 多级相对路径
  (check (path->string (path-parent (path "a/b/c")))
    =>
    (string-append "a" sep "b")
  ) ;check

  (when (not (os-windows?))
    ;; 根的 parent 是根本身(pathlib 语义)
    (check (path->string (path-parent (path-root))) => "/")
    ;; 尾斜杠先归一化:/tmp/ 的 parent 是 /
    (check (path->string (path-parent (path "/tmp/"))) => "/")
    ;; 绝对路径去末段,不含尾斜杠
    (check (path->string (path-parent (path "/tmp/demo.txt"))) => "/tmp")
    ;; 逐级回溯到根
    (check (path->string (path-parent (path-parent (path "/tmp/demo.txt")))) => "/")
    ;; 单段绝对路径 /a 的 parent 是根
    (check (path->string (path-parent (path "/a"))) => "/")
  ) ;when

  (when (os-windows?)
    ;; drive-absolute 单段:C:\Users 的 parent 是 C:\(保留 drive+root anchor)
    (check (path->string (path-parent (path "C:\\Users"))) => "C:\\")
    ;; 相对路径去末段,不含尾斜杠
    (check (path->string (path-parent (path "a\\b"))) => "a")
    ;; s7 的 port-filename / argv 在 Windows 上返回正斜杠绝对路径,
    ;; path-parent 必须同时识别 / 和 \(基于 record parts,自动归一化)
    (check (path->string (path-parent (path "C:/Users/foo/bar.scm")))
      =>
      "C:\\Users\\foo"
    ) ;check
    ;; string 输入同样工作(path-parent 内部先转 path)
    (check (path->string (path-parent "C:/Users/foo/bar.scm")) => "C:\\Users\\foo")
    ;; UNC 下 path-parent 剥到 share anchor
    (check (path->string (path-parent (path "\\\\srv\\sh\\a\\b")))
      =>
      "\\\\srv\\sh\\a"
    ) ;check
    ;; UNC 单段:parent 为 share anchor 自身(\\srv\sh\a → \\srv\sh)
    (check (path->string (path-parent (path "\\\\srv\\sh\\a"))) => "\\\\srv\\sh")
    ;; UNC anchor 自身:parent 是自身(pathlib 语义,根/anchor 不再上溯)
    (check (path->string (path-parent (path "\\\\srv\\sh"))) => "\\\\srv\\sh")
    ;; UNC 多段:剥到 share anchor 的上一段
    (check (path->string (path-parent (path "\\\\srv\\sh\\a\\b")))
      =>
      "\\\\srv\\sh\\a"
    ) ;check
    ;; drive-relative 多段:C:foo\bar → C:foo(保留 drive,去掉末段)
    (check (path->string (path-parent (path "C:foo\\bar"))) => "C:foo")
    ;; drive-relative 单段:C:foo → C:(pathlib 语义,保留 drive、root 空)
    (check (path->string (path-parent (path "C:foo"))) => "C:")
    ;; drive-absolute 根自身:C:\ 的 parent 是自身
    (check (path->string (path-parent (path "C:\\"))) => "C:\\")
    ;; 当前盘根单段:\foo 的 parent 是当前盘根 \
    (check (path->string (path-parent (path "\\foo"))) => "\\")
  ) ;when
) ;let

(check-report)
