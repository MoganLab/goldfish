;; chain-def-id
;; 获取链定义的唯一标识。
;;
;; 语法
;; ----
;; (chain-def-id chain)
;;
;; 参数
;; ----
;; chain : <chain-def>
;; 链定义记录。
;;
;; 返回值
;; ----
;; string
;; 链唯一标识。

(import (scheme base) (liii check) (liii pi code all-in-one types))

(check-set-mode! 'report-failed)

(let ((c (make-chain-def "coding-deep" "深度链" '())))
  (check (chain-def-id c) => "coding-deep")
) ;let

(check-report)
