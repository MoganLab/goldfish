;; check-type-hint-except
;; 检查全部 hint 库，但跳过已检查过的文件，避免单文件路径与全局扫描重复报告。
;;
;; 语法
;; ----
;; (check-type-hint-except checked-files)
;;
;; 参数
;; ----
;; checked-files : list of string  已由单文件路径检查过的文件路径
;;
;; 返回值
;; ------
;; list of violation
;; 未跳过文件的违规列表。

(import (scheme base) (liii check) (liii goldcheck type-hint))

(check-set-mode! 'report-failed)

;; 1. 全部跳过时只保留跨文件重复登记检查（仓库合规时为空）
(let ((viols (check-type-hint-except (scan-hint-libs))))
  (check viols => '())
) ;let

;; 2. 跳过列表为空时等价于全量检查（结构违规部分）
(let ((viols (check-type-hint-except '())))
  (check-true (list? viols))
) ;let

(check-report)
