(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

;; path-match
;; glob 模式匹配路径末段(对齐 pathlib.PurePath.match 的末段语义)。
;;
;; 语法
;; ----
;; (path-match path-value pattern) -> boolean
;;
;; 参数
;; ----
;; pattern : string?
;; 支持 * (任意字符) / ? (单字符) / [seq] / [^seq] / [a-z] 字符集。
;;
;; 返回值
;; ------
;; boolean?

(when (not (os-windows?))
  (check-true (path-match (path "foo.txt") "*.txt"))
  (check-true (path-match (path "/a/b/foo.txt") "*.txt"))
  (check-false (path-match (path "foo.py") "*.txt"))
  (check-true (path-match (path "foo.txt") "foo.???"))
  (check-false (path-match (path "foo.tx") "foo.???"))
  ;; 精确末段
  (check-true (path-match (path "a/b/c") "c"))
  (check-false (path-match (path "a/b/c") "b"))
  ;; 字符集
  (check-true (path-match (path "foo.txt") "*.[ct]xt"))
  ;; 含分隔符模式:从右匹配尾部若干段(对齐 pathlib.match)
  (check-true (path-match (path "a/b/c") "b/c"))
  (check-true (path-match (path "a/b/c") "a/b/c"))
  (check-true (path-match (path "a/b/c") "*/c"))
  (check-true (path-match (path "a/b/foo.txt") "b/*.txt"))
  (check-false (path-match (path "a/b/foo.txt") "a/*.txt"))
  ;; 绝对模式(以 / 开头):要求段数完全匹配
  (check-true (path-match (path "/a/b/c") "/a/b/c"))
  (check-true (path-match (path "/a/b/c") "a/b/c"))
  (check-false (path-match (path "/x/a/b/c") "/a/b/c"))
) ;when

;; Windows: match 大小写不敏感(对齐 pathlib PureWindowsPath.match)
(when (os-windows?)
  (check-true (path-match (path "Foo.TXT") "*.txt"))
  (check-true (path-match (path "C:\\a\\b\\foo.txt") "*.txt"))
  (check-true (path-match (path "FOO.txt") "foo.TXT"))
  (check-true (path-match (path "foo.TXt") "FOO.txt"))
  ;; 通配 + 大小写不敏感
  (check-true (path-match (path "Readme.MD") "*.md"))
) ;when

(check-report)
