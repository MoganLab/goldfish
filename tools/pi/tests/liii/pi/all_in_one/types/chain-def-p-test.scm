;; chain-def?
;; 判定对象是否为链定义记录。
;;
;; 语法
;; ----
;; (chain-def? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查对象。
;;
;; 返回值
;; ----
;; boolean
;; 为链定义时返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi all_in_one types))

(check-set-mode! 'report-failed)

(check-true (chain-def? (make-chain-def "id" "name" '())))
(check-false (chain-def? "not-a-chain"))
(check-false (chain-def? 456))

(check-report)
