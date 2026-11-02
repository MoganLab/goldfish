;; scan-hint-libs
;; 扫描所有类型提示库文件路径（通过 (liii goldcheck) 导出）。
;;
;; 语法
;; ----
;; (scan-hint-libs)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; list of string
;; 类型提示库文件的相对路径列表。

(import (scheme base) (liii check) (liii goldcheck))

(check-set-mode! 'report-failed)

;; 1. 扫描返回列表
(let ((libs (scan-hint-libs)))
  (check-true (list? libs))
) ;let

(check-report)
