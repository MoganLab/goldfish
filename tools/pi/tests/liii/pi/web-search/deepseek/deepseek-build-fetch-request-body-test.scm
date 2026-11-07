;; deepseek-build-fetch-request-body
;; 构建 deepseek 页面抓取接口的请求体。
;;
;; 语法
;; ----
;; (deepseek-build-fetch-request-body url)
;;
;; 参数
;; ----
;; url : string?
;; 待抓取页面的 URL。
;;
;; 返回值
;; ----
;; list?
;; 请求体字段 alist。

(import (liii check)
        (liii pi web-search deepseek)
) ;import

(check-set-mode! 'report-failed)

;; 请求体仅包含 url 字段
(check (deepseek-build-fetch-request-body "https://example.com")
  => '(("url" . "https://example.com"))
) ;check

(check-report)
