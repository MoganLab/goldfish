;; replace-last-segment
;; 返回把末段替换为新名字后的新路径。
;;
;; 语法
;; ----
;; (replace-last-segment path-value new-name) -> path?
;;
;; 参数
;; ----
;; path-value : path?
;; 输入路径。
;; new-name : string?
;; 替换后的末段名字。
;;
;; 返回值
;; ----
;; path?
;; 保留 drive/root/其他段、仅替换末段的新路径。
;;
;; 描述
;; ----
;; 替换后 Windows drive/anchor 保留（如 C:\a\b.txt → C:\a\c.md）；
;; 空路径（parts 为空或仅 "."）直接返回单段 new-name 的相对路径。

(import (liii check) (liii path) (liii path component) (liii os))

(check-set-mode! 'report-failed)

;; 带根锚点的用例：分隔符按平台输出
(when (not (os-windows?))
  (check (path->string (replace-last-segment (path "/a/b.txt") "c.md"))
    =>
    "/a/c.md"
  ) ;check
  ;; 保留根锚点
  (check (path->string (replace-last-segment (path "/b.txt") "z")) => "/z")
) ;when

(when (os-windows?)
  (check (path->string (replace-last-segment (path "C:\\a\\b.txt") "c.md"))
    =>
    "C:\\a\\c.md"
  ) ;check
  ;; 保留根锚点
  (check (path->string (replace-last-segment (path "\\b.txt") "z")) => "\\z")
) ;when

(check (path->string (replace-last-segment (path "b.txt") "c.md")) => "c.md")

;; 空路径返回单段相对路径
(check (path->string (replace-last-segment (path "") "x")) => "x")
(check (path->string (replace-last-segment (path ".") "x")) => "x")

(check-report)
