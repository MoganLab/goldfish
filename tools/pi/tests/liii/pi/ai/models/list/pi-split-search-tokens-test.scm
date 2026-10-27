(import (liii check)
        (liii pi ai models list))

(check-set-mode! 'report-failed)

;; pi-split-search-tokens
;; 将模型搜索字符串切分为 token 列表。
;;
;; 语法
;; ----
;; (pi-split-search-tokens str)
;;
;; 参数
;; ----
;; str : string
;; 以空白或 / 分隔的搜索字符串。
;;
;; 返回值
;; ----
;; list
;; 非空 token 字符串列表；非字符串输入返回空列表。
;;
;; 说明
;; ----
;; 连续分隔符和首尾分隔符不产生空 token。
;;
;; 示例
;; ----
;; (pi-split-search-tokens "google / gemini-2.5") => '("google" "gemini-2.5")

(check (pi-split-search-tokens "gemini flash") => '("gemini" "flash"))
(check (pi-split-search-tokens "google/gemini") => '("google" "gemini"))
(check (pi-split-search-tokens "  g25f  ") => '("g25f"))
(check (pi-split-search-tokens "google / gemini-2.5 ") => '("google" "gemini-2.5"))
(check (pi-split-search-tokens "") => '())
(check (pi-split-search-tokens #f) => '())

;; 非字符串、混合空白及连续分隔符的边界行为
(check (pi-split-search-tokens 42) => '())
(check (pi-split-search-tokens "google\t/\n gemini  flash") => '("google" "gemini" "flash"))
(check (pi-split-search-tokens "/// \t\n") => '())

(check-report)
