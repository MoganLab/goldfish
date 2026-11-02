;; check-unit-test-dirs
;; 检查指定文件集合所属的测试目录规范（通过 (liii goldcheck unit-test) 导出）。
;;
;; 语法
;; ----
;; (check-unit-test-dirs files)
;;
;; 参数
;; ----
;; files : list of string   源码文件或测试文件路径列表
;;
;; 返回值
;; ------
;; list of violation
;; 违规信息列表，全量合规时返回空列表。

(import (scheme base) (liii check) (liii goldcheck unit-test))

(check-set-mode! 'report-failed)

;; 1. 空文件列表返回空违规
(check (check-unit-test-dirs '()) => '())

(check-report)
