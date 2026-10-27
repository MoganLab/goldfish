(import (liii check)
        (liii pi ai models catalog))

(check-set-mode! 'report-failed)

;; pi-catalog-provider-names
;; 获取模型目录中不重复的 provider 名称。
;;
;; 语法
;; ----
;; (pi-catalog-provider-names catalog)
;;
;; 参数
;; ----
;; catalog : alist
;; provider 到模型数据的目录，键可以是 string 或 symbol。
;;
;; 返回值
;; ----
;; list
;; 按字典序排序的 provider 名称字符串列表。
;;
;; 说明
;; ----
;; string 和 symbol 键统一转换为字符串后去重，空目录返回空列表。
;;
;; 示例
;; ----
;; (pi-catalog-provider-names '(("google" . ()) (anthropic . ()))) => '("anthropic" "google")

(let* ((catalog
        (list
          (cons "google" '())
          (cons "anthropic" '())
          (cons 'deepseek '())
          (cons "kimi-coding" '())))
       (names (pi-catalog-provider-names catalog)))
  (check names => '("anthropic" "deepseek" "google" "kimi-coding")))

;; 同一 provider 的 string/symbol 键只保留一个名称
(check (pi-catalog-provider-names '(("google" . ()) (google . ()) ("google" . ())))
  =>
  '("google"))

;; 空 catalog 返回空列表
(check (pi-catalog-provider-names '()) => '())

(check-report)
