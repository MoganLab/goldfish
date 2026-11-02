;; check-type-hint-all
;; 扫描并检查全量类型提示库的合规性。
;;
;; 语法
;; ----
;; (check-type-hint-all)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; list of violation
;; 违规信息列表，全量合规时返回空列表。

(import (scheme base) (liii check) (liii goldcheck type-hint))

(check-set-mode! 'report-failed)

;; 1. 执行全量类型提示库检查
(let ((viols (check-type-hint-all)))
  (check-true (list? viols))
) ;let

(check-report)
