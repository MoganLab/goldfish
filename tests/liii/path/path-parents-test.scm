(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

;; path-parents
;; 获取所有祖先路径(对齐 pathlib .parents),返回向量。
;;
;; 语法
;; ----
;; (path-parents path-value) -> vector
;;
;; 返回值
;; ------
;; vector
;; 从最近父路径到最远(根或当前目录)的 path 对象序列。
;; 绝对路径终止于根;相对路径终止于当前目录 "."(对齐 pathlib)。

(when (not (os-windows?))
  ;; 绝对多段:/a/b/c → (/a/b /a /)
  (check (vector->list (vector-map path->string (path-parents (path "/a/b/c"))))
    =>
    '("/a/b" "/a" "/")
  ) ;check
  ;; 两段:/a/b → (/a /)
  (check (vector->list (vector-map path->string (path-parents (path "/a/b"))))
    =>
    '("/a" "/")
  ) ;check
  ;; 单段绝对:/a → (/)
  (check (vector->list (vector-map path->string (path-parents (path "/a"))))
    =>
    '("/")
  ) ;check
  ;; 相对多段:a/b/c → (a/b a .)(对齐 pathlib,末元素为当前目录)
  (check (vector->list (vector-map path->string (path-parents (path "a/b/c"))))
    =>
    '("a/b" "a" ".")
  ) ;check
  ;; 相对单段:a → (.)(对齐 pathlib)
  (check (vector->list (vector-map path->string (path-parents (path "a"))))
    =>
    '(".")
  ) ;check
  ;; 当前目录自身:. → ()(无祖先)
  (check (vector-length (path-parents (path "."))) => 0)
) ;when

(when (os-windows?)
  ;; drive-absolute:C:\a\b → (C:\a C:\)
  (check (vector->list (vector-map path->string (path-parents (path "C:\\a\\b"))))
    =>
    '("C:\\a" "C:\\")
  ) ;check
  ;; drive-absolute 单段:C:\a → (C:\)
  (check (vector->list (vector-map path->string (path-parents (path "C:\\a"))))
    =>
    '("C:\\")
  ) ;check
  ;; drive-absolute 根自身:C:\ → ()(anchor 不再上溯)
  (check (vector-length (path-parents (path "C:\\"))) => 0)
  ;; drive-relative:C:foo\bar → (C:foo C:)(pathlib 对齐)
  (check (vector->list (vector-map path->string (path-parents (path "C:foo\\bar"))))
    =>
    '("C:foo" "C:")
  ) ;check
  ;; UNC 多段:\\srv\sh\a\b → (\\srv\sh\a \\srv\sh\)(对齐 pathlib,末元素 anchor 带尾斜杠)
  (check (vector->list (vector-map path->string (path-parents (path "\\\\srv\\sh\\a\\b")))
         ) ;vector->list
    =>
    '("\\\\srv\\sh\\a" "\\\\srv\\sh\\")
  ) ;check
  ;; UNC 单段:\\srv\sh\a → (\\srv\sh\)
  (check (vector->list (vector-map path->string (path-parents (path "\\\\srv\\sh\\a"))))
    =>
    '("\\\\srv\\sh\\")
  ) ;check
  ;; UNC anchor 自身:\\srv\sh → ()(无祖先)
  (check (vector-length (path-parents (path "\\\\srv\\sh"))) => 0)
  ;; 当前盘根单段:\foo → (\)
  (check (vector->list (vector-map path->string (path-parents (path "\\foo"))))
    =>
    '("\\")
  ) ;check
) ;when

(check-report)
