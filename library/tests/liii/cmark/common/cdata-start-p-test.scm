;; cdata-start?
;; 判断位置 pos 是否为 CDATA 节起始 <![CDATA[。
;;
;; 语法
;; ----
;; (cdata-start? str pos)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; pos : integer
;; 起始位置。
;;
;; 返回值
;; ----
;; boolean
;; 匹配返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark common))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (cdata-start? "<![CDATA[x]]>" 0))
(check-false (cdata-start? "<div>" 0))

(check-report)
