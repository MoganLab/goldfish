;; normalize-extension
;; 规范化文件后缀名：缺少前导点时补上 "."。
;;
;; 语法
;; ----
;; (normalize-extension ext)
;;
;; 参数
;; ----
;; ext : string? 文件后缀名
;;
;; 返回值
;; ------
;; string? 带前导点的文件后缀名

(import (liii check) (liii base) (liii goldfix cli))

(check-set-mode! 'report-failed)

;; 测试：缺少前导点时补上
(check (normalize-extension "scm") => ".scm")

;; 测试：已有前导点时保持不变
(check (normalize-extension ".scm") => ".scm")

(check-report)
