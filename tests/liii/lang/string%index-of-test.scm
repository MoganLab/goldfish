;; string%index-of
;; 查找字符首次出现的下标（对标 Scala String.indexOf）。
;;
;; 语法
;; ----
;; ($ s :index-of c)
;;
;; 参数
;; ----
;; c : char?
;; 待查找字符。
;;
;; s : string?
;; 源字符串。
;;
;; 返回值
;; ------
;; integer?
;; 首次出现的下标（0 起，UTF-8 字符序）；不存在返回 -1。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ "ab" :index-of #\b) => 1)
(check ($ "测试" :index-of #\试) => 1)
(check ($ "ab" :index-of #\z) => -1)
(check ($ "" :index-of #\a) => -1)

(check-report)
