;; deepseek-build-search-request-body
;; 构建 deepseek 搜索接口的请求体。
;;
;; 语法
;; ----
;; (deepseek-build-search-request-body query)
;;
;; 参数
;; ----
;; query : string?
;; 搜索关键词。
;;
;; 返回值
;; ----
;; list?
;; 请求体字段 alist。

(import (liii check) (liii pi web-search deepseek))

(check-set-mode! 'report-failed)

;; 构建 deepseek 搜索接口的请求体。
(check (deepseek-build-search-request-body "hello")
  =>
  '(("query" . "hello") ("count" . 10))
) ;check

(check-report)
