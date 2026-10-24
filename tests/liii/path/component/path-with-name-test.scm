(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

;; path-with-name
;; 替换路径的末段名称(对齐 pathlib with_name)。
;;
;; 语法
;; ----
;; (path-with-name path-value name)
;; name: 新的末段名称
;;
;; 返回值
;; ------
;; path-value
;; 保留原路径的 drive/root/其他段。

(when (not (os-windows?))
  (check (path->string (path-with-name (path "/a/b.txt") "c.md")) => "/a/c.md")
  (check (path->string (path-with-name (path "x.txt") "y")) => "y")
) ;when

(when (os-windows?)
  ;; drive-absolute 路径改末段,保留 "C:\"
  (check (path->string (path-with-name (path "C:\\a\\b.txt") "c.md"))
    =>
    "C:\\a\\c.md"
  ) ;check
) ;when

(check-report)
