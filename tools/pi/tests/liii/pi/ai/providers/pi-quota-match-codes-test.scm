(import (liii check) (liii pi ai providers quota))

(check-set-mode! 'report-failed)

;; pi-quota-match-codes
;;
;; 候选标识列表与 provider 码表匹配，返回 (标识 . 额度类型) 或 #f。

(define table '(("1308" . "five-hour") ("1310" . "weekly")))

;; 1. 命中第一个可匹配的标识
(check (pi-quota-match-codes '("1310") table) => (cons "1310" "weekly"))
(check (pi-quota-match-codes '("9999" "1308") table)
  =>
  (cons "1308" "five-hour")
) ;check

;; 2. 都不命中返回 #f
(check (pi-quota-match-codes '("9999") table) => #f)
(check (pi-quota-match-codes '() table) => #f)

;; 3. 空码表永不命中
(check (pi-quota-match-codes '("1310") '()) => #f)

;; 4. 按精确字符串匹配（不做前缀/数字模糊匹配）
(check (pi-quota-match-codes '("131") table) => #f)

(check-report)
