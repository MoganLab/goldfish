;; %standard-sections
;; 单元测试文档注释中允许的标准小节标题列表。
;;
;; 语法
;; ----
;; %standard-sections
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; list of string
;; 允许的标准小节标题字符串列表。

(import (scheme base) (liii check) (liii goldcheck unit-test))

(check-set-mode! 'report-failed)

;; 1. 验证标准小节包含语法、参数、返回值等
(check-true (and (member "语法" %standard-sections string=?) #t))
(check-true (and (member "参数" %standard-sections string=?) #t))
(check-true (and (member "返回值" %standard-sections string=?) #t))
(check-true (and (member "错误处理" %standard-sections string=?) #t))

(check-report)
