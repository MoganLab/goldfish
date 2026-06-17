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

(check-report)
