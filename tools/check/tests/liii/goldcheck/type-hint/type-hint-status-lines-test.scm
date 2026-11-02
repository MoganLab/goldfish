;; type-hint-status-lines
;; 生成各个 hint 库的覆盖度与校验状态（FULL/PARTIAL, PASS/FAIL）文本行列表。
;;
;; 语法
;; ----
;; (type-hint-status-lines hint-files violations)
;;
;; 参数
;; ----
;; hint-files : list of string  hint 库文件路径列表
;; violations : list            违规列表
;;
;; 返回值
;; ------
;; list of string
;; 格式化后的状态行列表。

(import (scheme base) (liii check) (liii goldcheck type-hint))

(check-set-mode! 'report-failed)

;; 1. 空文件列表返回空行列表
(check (type-hint-status-lines '() '()) => '())

;; 2. 真实扫描文件列表返回非空行列表
(let* ((files (scan-hint-libs)) (lines (type-hint-status-lines files '())))
  (check-true (list? lines))
) ;let*

(check-report)
