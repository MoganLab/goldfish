(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

;; path-relative-to
;; 计算 path 相对于 base 的相对路径(对齐 pathlib.relative_to)。
;;
;; 语法
;; ----
;; (path-relative-to path-value base) -> path-value
;;
;; 返回值
;; ------
;; path-value
;; 若 path 不在 base 之下(段前缀不匹配 / anchor 不同),抛错。

(when (not (os-windows?))
  ;; 简单前缀
  (check (path->string (path-relative-to (path "/a/b/c") (path "/a"))) => "b/c")
  ;; base 等于 path → 当前目录
  (check (path->string (path-relative-to (path "/a/b") (path "/a/b"))) => ".")
  ;; root 作为 base
  (check (path->string (path-relative-to (path "/a/b") (path "/"))) => "a/b")
  ;; 相对路径
  (check (path->string (path-relative-to (path "a/b/c") (path "a/b"))) => "c")

  ;; 不相对时报错(不同子树)
  (check (guard (ex (else 'caught))
           (path->string (path-relative-to (path "/x/y") (path "/a")))
         ) ;guard
    =>
    'caught
  ) ;check
  ;; anchor 不同:绝对 vs 相对
  (check (guard (ex (else 'caught))
           (path->string (path-relative-to (path "/a/b") (path "a")))
         ) ;guard
    =>
    'caught
  ) ;check
) ;when

(when (os-windows?)
  ;; 同 drive 同 root,前缀匹配
  (check (path->string (path-relative-to (path "C:\\a\\b\\c") (path "C:\\a")))
    =>
    "b\\c"
  ) ;check
  ;; base 等于 path → 当前目录
  (check (path->string (path-relative-to (path "C:\\a\\b") (path "C:\\a\\b")))
    =>
    "."
  ) ;check
  ;; drive-absolute 根作为 base
  (check (path->string (path-relative-to (path "C:\\a\\b") (path "C:\\")))
    =>
    "a\\b"
  ) ;check
  ;; 不同 drive 报错(pathlib ValueError)
  (check (guard (ex (else 'caught))
           (path->string (path-relative-to (path "C:\\a") (path "D:\\")))
         ) ;guard
    =>
    'caught
  ) ;check
  ;; drive-absolute vs drive-relative(C:\ vs C:)anchor 不同(root 不同)报错
  (check (guard (ex (else 'caught))
           (path->string (path-relative-to (path "C:\\a") (path "C:")))
         ) ;guard
    =>
    'caught
  ) ;check
  ;; UNC 内部相对
  (check (path->string (path-relative-to (path "\\\\srv\\sh\\a\\b") (path "\\\\srv\\sh"))
         ) ;path->string
    =>
    "a\\b"
  ) ;check
  ;; 不同 UNC anchor 报错
  (check (guard (ex (else 'caught))
           (path->string (path-relative-to (path "\\\\srv1\\sh\\a") (path "\\\\srv2\\sh")))
         ) ;guard
    =>
    'caught
  ) ;check
  ;; UNC vs drive 报错
  (check (guard (ex (else 'caught))
           (path->string (path-relative-to (path "\\\\srv\\sh\\a") (path "C:\\")))
         ) ;guard
    =>
    'caught
  ) ;check
) ;when

(check-report)
