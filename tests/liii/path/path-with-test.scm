(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

;; path-with-suffix / path-with-name / path-with-stem
;; 替换末段的后缀/名称/stem(对齐 pathlib with_suffix/with_name/with_stem)。
;;
;; 语法
;; ----
;; (path-with-suffix path-value ext)   ext: 后缀字符串(含前导 ".")或 "" 去后缀
;; (path-with-name path-value name)    name: 新的末段名称
;; (path-with-stem path-value stem)    stem: 新的 stem(保留原后缀)
;;
;; 返回值
;; ------
;; path-value

(when (not (os-windows?))
  ;; with-suffix
  (check (path->string (path-with-suffix (path "a.txt") ".md")) => "a.md")
  ;; 无后缀文件加后缀
  (check (path->string (path-with-suffix (path "README") ".md")) => "README.md")
  ;; 多后缀只替换最后一个
  (check (path->string (path-with-suffix (path "a.tar.gz") ".md")) => "a.tar.md")
  ;; 去后缀
  (check (path->string (path-with-suffix (path "a.txt") "")) => "a")
  ;; 带目录
  (check (path->string (path-with-suffix (path "/tmp/a.txt") ".md"))
    =>
    "/tmp/a.md"
  ) ;check
  ;; 隐藏文件加后缀:.bashrc + .bak → .bashrc.bak
  (check (path->string (path-with-suffix (path ".bashrc") ".bak"))
    =>
    ".bashrc.bak"
  ) ;check

  ;; with-name
  (check (path->string (path-with-name (path "/a/b.txt") "c.md")) => "/a/c.md")
  (check (path->string (path-with-name (path "x.txt") "y")) => "y")

  ;; with-stem(保留后缀)
  (check (path->string (path-with-stem (path "a.txt") "b")) => "b.txt")
  (check (path->string (path-with-stem (path "/tmp/a.tar.gz") "new"))
    =>
    "/tmp/new.tar.gz"
  ) ;check
) ;when

(check-report)
