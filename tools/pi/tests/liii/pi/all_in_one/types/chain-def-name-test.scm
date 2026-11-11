;; chain-def-name
;; 获取链定义的展示名称。
;;
;; 语法
;; ----
;; (chain-def-name chain)
;;
;; 参数
;; ----
;; chain : <chain-def>
;; 链定义记录。
;;
;; 返回值
;; ----
;; string
;; 链展示名称。

(import (scheme base) (liii check) (liii pi all_in_one types))

(check-set-mode! 'report-failed)

(let ((c (make-chain-def "coding-deep" "深度链" '())))
  (check (chain-def-name c) => "深度链")
) ;let

(check-report)
