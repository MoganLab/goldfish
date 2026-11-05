;; pi-block?
;; 判断对象是否为内容块 record。
;;
;; 语法
;; ----
;; (pi-block? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待检验的对象。
;;
;; 返回值
;; ----
;; boolean
;; 当为 text、thinking 或 toolCall 内容块时返回 #t。

(import (liii check) (liii pi ai message record))

(check-set-mode! 'report-failed)

(check-true (pi-block? (pi-text-block "txt")))
(check-true (pi-block? (pi-thinking-block "think")))
(check-false (pi-block? "string"))

(check-report)
