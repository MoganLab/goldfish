(import (liii check) (liii pi ai providers quota))

(check-set-mode! 'report-failed)

;; pi-quota-match-codes
;; 把候选错误标识列表与某个 provider 的码表做匹配。
;;
;; 语法
;; ----
;; (pi-quota-match-codes codes table)
;;
;; 参数
;; ----
;; codes : list?
;; 候选标识字符串列表，通常来自 pi-quota-json-fields 的 car。
;;
;; table : list?
;; 码表 ((标识 . 额度类型) …)，由 provider 自己定义。
;;
;; 返回值
;; ----
;; pair 或 #f
;; 命中返回 (标识 . 额度类型)，都不命中返回 #f。
;;
;; 注意
;; ----
;; 按精确字符串匹配，不做前缀或数字模糊匹配；码表为空时永不命中。

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
