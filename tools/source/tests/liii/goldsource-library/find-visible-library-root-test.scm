;; find-visible-library-root
;; 在当前 *load-path* 中查找给定库查询对应的根目录。
;;
;; 语法
;; ----
;; (find-visible-library-root query)
;;
;; 参数
;; ----
;; query : string?
;; 形如 "组织/库名" 的库查询字符串。
;;
;; 返回值
;; ------
;; string? | #f
;; 若库在 *load-path* 中存在则返回其根目录路径字符串，否则返回 #f。

(import (scheme base) (liii check) (liii path) (liii goldsource-library))

(check-set-mode! 'report-failed)

;; 测试：查找已存在的库根目录
(let ((load-root (find-visible-library-root "liii/string")))
  (check-true (string? load-root))
  (check-true (path-file? (path-join load-root "liii" "string.scm")))
) ;let

;; 测试：不存在的库返回 #f
(check (find-visible-library-root "liii/not-a-real-library") => #f)

;; 测试：非法查询格式返回 #f
(check (find-visible-library-root "string") => #f)
(check (find-visible-library-root "") => #f)
(check (find-visible-library-root 123) => #f)

(check-report)
