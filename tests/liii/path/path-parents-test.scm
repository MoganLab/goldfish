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
;; 绝对路径终止于根;相对路径终止于最浅段(不再补 ".")。

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
  ;; 相对多段:a/b/c → (a/b a)
  (check (vector->list (vector-map path->string (path-parents (path "a/b/c"))))
    =>
    '("a/b" "a")
  ) ;check
  ;; 相对单段:a → ()(无祖先)
  (check (vector-length (path-parents (path "a"))) => 0)
) ;when

(when (os-windows?)
  ;; drive-absolute:C:\a\b → (C:\a C:\)
  (check (vector->list (vector-map path->string (path-parents (path "C:\\a\\b"))))
    =>
    '("C:\\a" "C:\\")
  ) ;check
) ;when

(check-report)
