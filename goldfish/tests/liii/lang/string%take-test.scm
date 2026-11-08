;; string%take
;; 取字符串前 n 个字符，越界宽容（对标 Scala String.take）。
;;
;; 语法
;; ----
;; ($ s :take n)
;;
;; 参数
;; ----
;; n : integer?
;; 保留的前缀字符数，越界/负数宽容。
;;
;; s : string?
;; 源字符串。
;;
;; 返回值
;; ------
;; string?
;; s 的前 n 个字符；n 超过长度返回全部，n 为负返回空串
;; （Scala Oracle 验证，区别于 (liii string-cursor) string-take 的越界抛错）。
;;
;; 描述
;; ----
;; 按 UTF-8 字符数（非字节数）计数。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ "héllo" :take 2) => "hé")
(check ($ "abc" :take 10) => "abc")
(check ($ "héllo" :take -1) => "")
(check ($ "" :take 3) => "")
(check ($ "测试法" :take 2) => "测试")

(check-report)
