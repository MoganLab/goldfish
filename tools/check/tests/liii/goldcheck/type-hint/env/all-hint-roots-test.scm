;; all-hint-roots
;; 返回仓库全部 hint 根目录（带仓库根前缀），用于按 import 定位 hint。
;;
;; 语法
;; ----
;; (all-hint-roots)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; list of string
;; 各 domain 推导出的 hint 根目录绝对/仓库相对路径列表。

(import (scheme base)
  (liii check)
  (liii gfproject config)
  (liii goldcheck type-hint env)
) ;import

(check-set-mode! 'report-failed)

;; 1. 返回非空列表
(let ((roots (all-hint-roots)))
  (check-true (list? roots))
  (check-true (pair? roots))
) ;let

;; 2. 与 gfproject-hint-dirs 一一对应（带仓库根前缀）
(let ((roots (all-hint-roots)) (dirs (gfproject-hint-dirs)))
  (check (length roots) => (length dirs))
) ;let

(check-report)
