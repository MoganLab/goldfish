;; check-no-glue-duplicates
;; 检查同一 glue 是否被多个 define-library 重复使用。
;;
;; 语法
;; ----
;; (check-no-glue-duplicates file-paths)
;;
;; 参数
;; ----
;; file-paths : list of string   待检查的文件路径列表
;;
;; 返回值
;; ------
;; list of violation
;; 违规信息列表，无跨库重复使用时返回空列表。

(import (scheme base) (liii check) (liii goldcheck no-glue))

(check-set-mode! 'report-failed)

;; 1. 空文件列表无违规
(check (check-no-glue-duplicates '()) => '())

(check-report)
