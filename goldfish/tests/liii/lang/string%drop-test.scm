;; string%drop
;; 跳过字符串前 n 个字符，越界宽容（对标 Scala String.drop）。
;;
;; 语法
;; ----
;; ($ s :drop n)
;;
;; 参数
;; ----
;; n : integer?
;; 跳过的前缀字符数，越界/负数宽容。
;;
;; s : string?
;; 源字符串。
;;
;; 返回值
;; ------
;; string?
;; 去掉前 n 个字符后的新串；n 超过长度返回空串，n 为负返回全部
;; （Scala Oracle 验证）。
;;
;; 描述
;; ----
;; 按 UTF-8 字符数（非字节数）计数。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ "héllo" :drop 2) => "llo")
(check ($ "héllo" :drop 20) => "")
(check ($ "héllo" :drop -1) => "héllo")
(check ($ "测试法" :drop 1) => "试法")

(check-report)
