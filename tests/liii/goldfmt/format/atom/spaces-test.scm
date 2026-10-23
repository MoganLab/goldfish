(import (liii check))
(import (liii goldfmt format atom))

(check-set-mode! 'report-failed)

;; spaces
;; 生成 n 个空格组成的字符串。
;;
;; 语法
;; ----
;; (spaces n)
;;
;; 参数
;; ----
;; n : integer?
;;
;; 返回值
;; ------
;; string?
;; n 小于等于 0 时返回空字符串。

;; 测试：n 为正数生成对应数量空格
(check (spaces 3) => "   ")

;; 测试：n 为 0 返回空字符串
(check (spaces 0) => "")

;; 测试：n 为负数返回空字符串
(check (spaces -2) => "")

(check-report)
