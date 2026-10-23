(import (liii check))
(import (liii goldfmt format atom))

(check-set-mode! 'report-failed)

;; make-newlines
;; 将换行标记数量转换为实际的换行符串：n 个空行对应 n-1 个换行符。
;;
;; 语法
;; ----
;; (make-newlines n)
;;
;; 参数
;; ----
;; n : integer?
;; 空行数量。
;;
;; 返回值
;; ------
;; string?
;; n 小于等于 1 时返回空字符串，否则 n-1 个换行符。

;; 测试：n 为 1 返回空字符串（无额外空行）
(check (make-newlines 1) => "")

;; 测试：n 为 0 返回空字符串
(check (make-newlines 0) => "")

;; 测试：n 为 2 返回一个换行符
(check (make-newlines 2) => "\n")

;; 测试：n 为 3 返回两个换行符
(check (make-newlines 3) => "\n\n")

(check-report)
