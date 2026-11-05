;; source-library-path
;; 根据库查询字符串，结合当前 *load-path* 定位库源代码文件的路径。
;;
;; 语法
;; ----
;; (source-library-path query)
;;
;; 参数
;; ----
;; query : string?
;; 形如 "组织/库名" 的库查询字符串。
;;
;; 返回值
;; ------
;; string? | #f
;; 若定位成功返回源码文件路径字符串，否则返回 #f。

(import (scheme base) (liii check) (liii path) (liii goldsource-library))

(check-set-mode! 'report-failed)

;; 测试：定位已存在的库源码文件路径
(let ((source-path (source-library-path "liii/string")))
  (check-true (string? source-path))
  (check-true (path-file? source-path))
  (check (path-name source-path) => "string.scm")
) ;let

;; 测试：不存在的库返回 #f
(check (source-library-path "liii/not-a-real-library") => #f)

;; 测试：非法查询格式返回 #f
(check (source-library-path "string-split") => #f)
(check (source-library-path "") => #f)
(check (source-library-path 123) => #f)

(check-report)
