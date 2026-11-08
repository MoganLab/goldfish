;; string%length
;; 获取字符串字符数，UTF-8 语义（对标 Scala String.length）。
;;
;; 语法
;; ----
;; ($ s :length)
;;
;; 参数
;; ----
;; s : string?
;; 源字符串。
;;
;; 返回值
;; ------
;; integer?
;; UTF-8 字符数（非字节数）；空串为 0。
;;
;; 描述
;; ----
;; 基于 (liii string-cursor) 游标计数，中文/emoji 每个算 1。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ "测试" :length) => 2)
(check ($ "测试🎉" :length) => 3)
(check ($ "héllo" :length) => 5)
(check ($ "" :length) => 0)

(check-report)
