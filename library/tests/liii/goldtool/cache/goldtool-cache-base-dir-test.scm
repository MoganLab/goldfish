;; goldtool-cache-base-dir
;; 返回指定工具的共享缓存根目录，位于用户主目录的
;; .cache/goldfish/<工具名>/<版本号> 之下。
;;
;; 语法
;; ----
;; (goldtool-cache-base-dir tool-name)
;;
;; 参数
;; ----
;; tool-name : string?
;; 工具名称，作为缓存目录的隔离层级。
;;
;; 返回值
;; ------
;; string?
;; 缓存根目录的绝对路径。

(import (scheme base)
  (liii base)
  (liii check)
  (liii path)
  (liii string)
  (liii goldtool cache)
) ;import

(check-set-mode! 'report-failed)

;; 返回的是字符串路径
(check (string? (goldtool-cache-base-dir "goldtool-test")) => #t)

;; 路径以 .cache/goldfish/<工具名>/<版本号> 结尾
(check
  (string-ends? (path-as-posix (path (goldtool-cache-base-dir "goldtool-test")))
    (string-append ".cache/goldfish/goldtool-test/" (version))
  ) ;string-ends?
  =>
  #t
) ;check

;; 同一工具名返回同一目录，不同工具名互相隔离
(check (equal? (goldtool-cache-base-dir "tool-a") (goldtool-cache-base-dir "tool-a")) => #t)
(check (equal? (goldtool-cache-base-dir "tool-a") (goldtool-cache-base-dir "tool-b")) => #f)

(check-report)
