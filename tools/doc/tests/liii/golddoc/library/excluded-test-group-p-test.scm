;; excluded-test-group?
;; 判断测试分组是否应从文档搜索范围中排除。
;;
;; 语法
;; ----
;; (excluded-test-group? group)
;;
;; 参数
;; ----
;; group : string
;;   测试根目录下的分组名称。
;;
;; 返回值
;; ----
;; boolean?
;;   当前仅 goldfish 分组返回 #t，其他分组返回 #f。

(set! *load-path* (cons "tools/doc" *load-path*))

(import (liii check) (liii golddoc library))

(check-set-mode! 'report-failed)

(check (excluded-test-group? "goldfish") => #t)
(check (excluded-test-group? "liii") => #f)
(check (excluded-test-group? "srfi") => #f)
(check (excluded-test-group? "scheme") => #f)
(check (excluded-test-group? "") => #f)

(check-report)
