;; deepseek-build-fetch-endpoint
;; 构建 deepseek 页面抓取接口的完整 URL。
;;
;; 语法
;; ----
;; (deepseek-build-fetch-endpoint base-url)
;;
;; 参数
;; ----
;; base-url : string?
;; deepseek 服务基础 URL。
;;
;; 返回值
;; ----
;; string?
;; 抓取接口的完整 URL。

(import (liii check) (liii pi web-search deepseek))

(check-set-mode! 'report-failed)

;; 1. 基础 URL 直接拼接抓取路径
(check (deepseek-build-fetch-endpoint "https://api.deepseek.com")
  =>
  "https://api.deepseek.com/v1/tools/fetch"
) ;check

;; 2. 末尾斜杠被剥离
(check (deepseek-build-fetch-endpoint "https://api.deepseek.com/")
  =>
  "https://api.deepseek.com/v1/tools/fetch"
) ;check

(check-report)
