;; pi-pool-key-last-used
;; 读取密钥记录最近一次被使用的时间戳。
;;
;; 语法
;; ----
;; (pi-pool-key-last-used pk)
;;
;; 参数
;; ----
;; pk : pi-pool-key?
;; 密钥记录。
;;
;; 返回值
;; ----
;; number?
;; 最近使用时间（毫秒），初始值为 0。

(import (liii check) (liii pi multikey pool))

(check-set-mode! 'report-failed)

;; 1. 新建记录的时间戳为 0
(check (pi-pool-key-last-used (pi-pool-key "sk-alpha")) => 0)

;; 2. 更新时间戳后可读出
(let ((pk (pi-pool-key "sk-alpha")))
  (pi-pool-key-set-last-used! pk 1234)
  (check (pi-pool-key-last-used pk) => 1234)
) ;let

(check-report)
