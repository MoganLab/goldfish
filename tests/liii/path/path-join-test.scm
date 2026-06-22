(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

;; path-join
;; 拼接路径。
;;
;; 语法
;; ----
;; (path-join base seg1 seg2 ...) -> path-value
;;
;; 参数
;; ----
;; base : path-value | string?
;; 基础路径。
;; seg1, seg2, ... : string?
;; 要拼接的路径段。
;;
;; 返回值
;; ----
;; path-value
;; 返回拼接后的路径值。
;;
;; 描述
;; ----
;; path-join 是 rich-path 中 :/ 的函数式版本。

(let ((sep (string (os-sep))))
  ;; path-join 测试
  (check (path->string (path-join (path "tmp") "demo.txt"))
    =>
    (string-append "tmp" sep "demo.txt")
  ) ;check
  (check (path->string (path-join (path "tmp") "a" "b" "c.txt"))
    =>
    (string-append "tmp" sep "a" sep "b" sep "c.txt")
  ) ;check

  (when (not (os-windows?))
    (check (path->string (path-join (path-root) "tmp" "demo.txt"))
      =>
      "/tmp/demo.txt"
    ) ;check
    (check-true (path-equals? (path-join (path-root) (path "tmp/demo.txt"))
                  (path "/tmp/demo.txt")
                ) ;path-equals?
    ) ;check-true

    ;; 绝对参数重置(pathlib 语义):/a join /b => /b
    (check (path->string (path-join (path "/a") "/b")) => "/b")
    ;; 相对参数追加到绝对 base:/a join b => /a/b
    (check (path->string (path-join (path "/a") "b")) => "/a/b")
    ;; 相对 base 遇绝对参数重置:c join /d => /d
    (check (path->string (path-join (path "c") "/d")) => "/d")
    ;; 中间绝对参数重置后续: join(/a b /c d) => /c/d
    (check (path->string (path-join (path "/a") "b" "/c" "d")) => "/c/d")
    ;; 无双斜杠:/a/ join b => /a/b(不带尾斜杠重复)
    (check (path->string (path-join (path "/a/") "b")) => "/a/b")
  ) ;when

  (when (os-windows?)
    ;; 相对 base + 相对段
    (check (path->string (path-join (path "tmp") "a" "b.txt")) => "tmp\\a\\b.txt")
    ;; drive-absolute base + 相对段
    (check (path->string (path-join (path "C:\\Users") "foo" "bar.scm"))
      =>
      "C:\\Users\\foo\\bar.scm"
    ) ;check
    ;; drive 重置(pathlib 语义):C:\a join D:\b => D:\b
    (check (path->string (path-join (path "C:\\a") "D:\\b")) => "D:\\b")
    ;; 绝对参数清空 base:C:\a join \b => \b
    ;; (current-drive root, path-absolute? 判定为绝对 → 重置 base。
    ;; 与 pathlib 有差异:Python 会保留 base 的 drive 给出 C:\b。当前 path-join
    ;; 的重置语义未区分 "drive 继承" 和 "完全重置",留作后续对齐任务。)
    (check (path->string (path-join (path "C:\\a") "\\b")) => "\\b")
    ;; drive-absolute base + 相对段,中间遇 drive-relative 段不重置
    (check (path->string (path-join (path "C:\\base") "D:rel" "x.txt"))
      =>
      "C:\\base\\D:rel\\x.txt"
    ) ;check
    ;; UNC base + 相对段
    (check (path->string (path-join (path "\\\\srv\\sh") "a" "b.txt"))
      =>
      "\\\\srv\\sh\\a\\b.txt"
    ) ;check
    ;; 正斜杠识别:Windows 上 / 和 \ 等价
    (check (path->string (path-join (path "C:/Users") "foo/bar.scm"))
      =>
      "C:\\Users\\foo\\bar.scm"
    ) ;check
    ;; string base(path-join 接受 path 或 string)
    (check (path->string (path-join "C:\\tmp" "x.txt")) => "C:\\tmp\\x.txt")
  ) ;when
) ;let

(check-report)
