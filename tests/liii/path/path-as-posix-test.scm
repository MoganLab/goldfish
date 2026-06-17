(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

;; path-as-posix
;; 返回用 / 作分隔符的字符串(对齐 pathlib.PurePath.as_posix)。
;;
;; 语法
;; ----
;; (path-as-posix path-value) -> string
;;
;; 返回值
;; ------
;; string
;; posix 下等价于 path->string;windows 下把 \ 转成 /。

(when (not (os-windows?))
  (check (path-as-posix (path "/a/b/c")) => "/a/b/c")
  (check (path-as-posix (path "a/b")) => "a/b")
) ;when

(when (os-windows?)
  ;; as-posix 把 \ 转 /
  (check (path-as-posix (path "C:\\a\\b")) => "C:/a/b")
) ;when

(check-report)
