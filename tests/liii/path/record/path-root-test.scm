(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

;; path-root
;; 字符串访问器,对齐 pathlib.PurePath.root。
;;
;; 语法
;; ----
;; (path-root path-value) -> string
;;
;; 参数
;; ----
;; path-value : path-value
;; 要查询的路径值。
;;
;; 返回值
;; ----
;; string
;; 有根分隔符: Windows "\" / posix "/"
;; 无根: ""

;; posix: root 字段为 #\/ 时返回 "/",否则 ""
(when (not (os-windows?))
  (check (path-root (path "/a")) => "/")
  (check (path-root (path "/a/b")) => "/")
  (check (path-root (path "/")) => "/")
  (check (path-root (path "a/b")) => "")
  (check (path-root (path ".")) => "")
  (check (path-root (path "..")) => "")
  (check (path-root (path "a/b/c")) => "")
  (check (path-root (path "a/")) => "")
) ;when

(when (os-windows?)
  ;; drive-absolute: C:\  -> root = "\"
  (check (path-root (path "C:\\a")) => "\\")
  (check (path-root (path "C:\\a\\b")) => "\\")
  (check (path-root (path "C:\\")) => "\\")
  ;; drive-relative: C:foo  -> root = ""
  (check (path-root (path "C:foo")) => "")
  (check (path-root (path "C:foo\\bar")) => "")
  ;; current-drive root: \foo  -> root = "\"
  (check (path-root (path "\\foo")) => "\\")
  (check (path-root (path "\\")) => "\\")
  ;; UNC: \\srv (only server, no share) -> root = ""
  (check (path-root (path "\\\\srv")) => "")
  ;; UNC: \\srv\sh ... -> root = "\"
  (check (path-root (path "\\\\srv\\sh")) => "\\")
  (check (path-root (path "\\\\srv\\sh\\a")) => "\\")
  (check (path-root (path "\\\\srv\\sh\\a\\b")) => "\\")
  (check (path-root (path "\\\\srv\\share\\a\\b")) => "\\")
  (check (path-root (path "\\\\srv\\share")) => "\\")
  ;; 相对路径: foo\bar -> root = ""
  (check (path-root (path "foo\\bar")) => "")
) ;when

(check-report)
