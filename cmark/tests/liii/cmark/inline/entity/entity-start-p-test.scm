;; entity-start?
;; 判断位置 pos 是否为 & 字符实体起始。
;;
;; 语法
;; ----
;; (entity-start? str pos)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; pos : integer
;; 位置索引。
;;
;; 返回值
;; ----
;; boolean
;; 以 & 开头返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline entity))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (entity-start? "&amp;" 0))
(check-false (entity-start? "amp" 0))

(check-report)
