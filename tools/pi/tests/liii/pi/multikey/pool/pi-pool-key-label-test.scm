;; pi-pool-key-label
;; 读取密钥记录中的标签。
;;
;; 语法
;; ----
;; (pi-pool-key-label pk)
;;
;; 参数
;; ----
;; pk : pi-pool-key?
;; 密钥记录。
;;
;; 返回值
;; ----
;; string?
;; 标签字符串，未设置时为空字符串。

(import (liii check)
        (liii pi multikey pool)
) ;import

(check-set-mode! 'report-failed)

;; 1. 未提供标签时为空字符串
(check (pi-pool-key-label (pi-pool-key "sk-alpha")) => "")

;; 2. 提供标签时返回该标签
(check (pi-pool-key-label (pi-pool-key "sk-alpha" "alpha-label")) => "alpha-label")

(check-report)
