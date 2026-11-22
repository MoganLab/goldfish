;; processing-instruction-start?
;; 判断位置 pos 是否为处理指令起始 <?。
;;
;; 语法
;; ----
;; (processing-instruction-start? str pos)
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

(check-true (processing-instruction-start? "<?xml?>" 0))
(check-false (processing-instruction-start? "<div>" 0))

(check-report)
