;; deepseek-build-search-endpoint
;; 构建 deepseek 搜索接口的完整 URL。
;;
;; 语法
;; ----
;; (deepseek-build-search-endpoint base-url)
;;
;; 参数
;; ----
;; base-url : string?
;; deepseek 服务基础 URL。
;;
;; 返回值
;; ----
;; string?
;; 搜索接口的完整 URL。

(import (liii check)
        (liii pi web-search deepseek)
) ;import

(check-set-mode! 'report-failed)

;; 1. 基础 URL 直接拼接搜索路径
(check (deepseek-build-search-endpoint "https://api.deepseek.com")
  => "https://api.deepseek.com/v1/tools/search"
) ;check

;; 2. 末尾斜杠被剥离
(check (deepseek-build-search-endpoint "https://api.deepseek.com/")
  => "https://api.deepseek.com/v1/tools/search"
) ;check

(check-report)
