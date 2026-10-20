(import (liii check)
        (liii pi models catalog))

(check-set-mode! 'report-failed)

;; pi-catalog-provider-names
;; 返回 catalog 中全部 provider 名称列表（字符串、字典序排序）

(let* ((catalog
        (list
          (cons "google" '())
          (cons "anthropic" '())
          (cons 'deepseek '())
          (cons "kimi-coding" '())))
       (names (pi-catalog-provider-names catalog)))
  (check names => '("anthropic" "deepseek" "google" "kimi-coding")))

;; 空 catalog 返回空列表
(check (pi-catalog-provider-names '()) => '())

(check-report)
