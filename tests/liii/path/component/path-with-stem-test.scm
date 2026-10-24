(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

;; path-with-stem
;; 替换末段 stem 并保留最后一个后缀(对齐 pathlib with_stem)。
;;
;; 语法
;; ----
;; (path-with-stem path-value stem)
;; stem: 新的 stem
;;
;; 返回值
;; ------
;; path-value
;; a.tar.gz → new.gz(只保留最后一个后缀)。

(when (not (os-windows?))
  (check (path->string (path-with-stem (path "a.txt") "b")) => "b.txt")
  (check (path->string (path-with-stem (path "/tmp/a.tar.gz") "new"))
    =>
    "/tmp/new.gz"
  ) ;check
) ;when

(when (os-windows?)
  ;; drive-absolute 路径改 stem,保留 "C:\" 与最后一个后缀
  (check (path->string (path-with-stem (path "C:\\tmp\\a.tar.gz") "new"))
    =>
    "C:\\tmp\\new.gz"
  ) ;check
) ;when

(check-report)
