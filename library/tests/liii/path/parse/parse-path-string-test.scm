;; parse-path-string
;; 将 posix/Windows 普通路径字符串解析为段向量与根。
;;
;; 语法
;; ----
;; (parse-path-string s)
;;
;; 参数
;; ----
;; s : string?
;; 路径字符串（无 UNC、无盘符）。
;;
;; 返回值
;; ----
;; values
;; 两个值：parts 段向量与 root 字符（或 #f）。
;;
;; 描述
;; ----
;; 起始分隔符由 root 字段表达，不混入 parts；
;; "." 段与空段被过滤。Windows 平台先把 / 规范化为 \。

(import (liii check) (liii os) (liii path parse) (srfi srfi-11))

(check-set-mode! 'report-failed)

(unless (os-windows?)
  (let-values (((parts root) (parse-path-string "")))
    (check parts => #("."))
    (check root => #f)
  ) ;let-values
  (let-values (((parts root) (parse-path-string ".")))
    (check parts => #("."))
    (check root => #f)
  ) ;let-values
  (let-values (((parts root) (parse-path-string "/")))
    (check parts => #())
    (check root => #\/)
  ) ;let-values
  (let-values (((parts root) (parse-path-string "a/b")))
    (check parts => #("a" "b"))
    (check root => #f)
  ) ;let-values
  (let-values (((parts root) (parse-path-string "/a/b")))
    (check parts => #("a" "b"))
    (check root => #\/)
  ) ;let-values
  (let-values (((parts root) (parse-path-string "/tmp/")))
    (check parts => #("tmp"))
    (check root => #\/)
  ) ;let-values
) ;unless

(check-report)
