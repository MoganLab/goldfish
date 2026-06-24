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
  ;; with-suffix 不以 "." 开头时报 value-error(对齐 pathlib ValueError)
  (check-catch 'value-error (path-with-suffix (path "a.txt") "md"))
  ;; with-suffix 去多后缀只去最后一个:a.tar.gz + "" => a.tar(对齐 pathlib)
  (check (path->string (path-with-suffix (path "a.tar.gz") "")) => "a.tar")

  ;; with-name
  (check (path->string (path-with-name (path "/a/b.txt") "c.md")) => "/a/c.md")
  (check (path->string (path-with-name (path "x.txt") "y")) => "y")

  ;; with-stem(只保留最后一个后缀,对齐 pathlib.with_stem)
  (check (path->string (path-with-stem (path "a.txt") "b")) => "b.txt")
  (check (path->string (path-with-stem (path "/tmp/a.tar.gz") "new"))
    =>
    "/tmp/new.gz"
  ) ;check
) ;when

(when (os-windows?)
  ;; drive-absolute 路径改后缀
  (check (path->string (path-with-suffix (path "C:\\tmp\\a.txt") ".md"))
    =>
    "C:\\tmp\\a.md"
  ) ;check
  ;; 去后缀
  (check (path->string (path-with-suffix (path "C:\\tmp\\a.txt") ""))
    =>
    "C:\\tmp\\a"
  ) ;check
  ;; 多后缀只替换最后一个
  (check (path->string (path-with-suffix (path "C:\\tmp\\a.tar.gz") ".md"))
    =>
    "C:\\tmp\\a.tar.md"
  ) ;check
  ;; 正斜杠输入(Windows 接受),输出统一反斜杠
  (check (path->string (path-with-suffix (path "C:/tmp/a.txt") ".md"))
    =>
    "C:\\tmp\\a.md"
  ) ;check
  ;; with-name
  (check (path->string (path-with-name (path "C:\\a\\b.txt") "c.md"))
    =>
    "C:\\a\\c.md"
  ) ;check
  ;; with-stem(只保留最后一个后缀,对齐 pathlib.with_stem)
  (check (path->string (path-with-stem (path "C:\\tmp\\a.tar.gz") "new"))
    =>
    "C:\\tmp\\new.gz"
  ) ;check
  ;; UNC 路径改后缀
  (check (path->string (path-with-suffix (path "\\\\srv\\sh\\a.txt") ".md"))
    =>
    "\\\\srv\\sh\\a.md"
  ) ;check
) ;when

(check-report)
