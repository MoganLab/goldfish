;; deepseek-append-fetched-page
;; 将抓取到的页面内容追加到搜索结果正文末尾。
;;
;; 语法
;; ----
;; (deepseek-append-fetched-page text page . opt-max-chars)
;;
;; 参数
;; ----
;; text : string?
;; 原始搜索结果文本。
;;
;; page : list?
;; 抓取到的页面数据，包含 title、url 与 content 字段。
;;
;; opt-max-chars : integer?（可选）
;; 页面正文最大字符数，缺省为 2000。
;;
;; 返回值
;; ----
;; string?
;; 追加页面内容后的文本。

(import (liii check) (liii string) (liii json) (liii pi web-search deepseek))

(check-set-mode! 'report-failed)

(define (page title url content)
  (string->json (string-append "{\"title\":\"" title "\",\"url\":\"" url
                  "\",\"content\":\"" content "\"}"
                ) ;string-append
  ) ;string->json
) ;define

;; 1. 空正文时直接以标题段落开头
(let ((out (deepseek-append-fetched-page ""
             (page "Example Page" "https://example.com" "body text")
           ) ;deepseek-append-fetched-page
      ) ;out
     ) ;
  (check-true (string-starts? out "## Fetched Pages"))
  (check-true (string-contains? out "### Example Page"))
  (check-true (string-contains? out "https://example.com"))
  (check-true (string-contains? out "body text"))
) ;let

;; 2. 非空正文时保留原文并追加段落
(let ((out (deepseek-append-fetched-page "search result" (page "T" "https://u" "c")))
     ) ;
  (check-true (string-starts? out "search result"))
  (check-true (string-contains? out "## Fetched Pages"))
  (check-true (string-contains? out "### T"))
) ;let

;; 3. 正文超长时截断
(let ((out (deepseek-append-fetched-page "" (page "T" "https://u" "0123456789") 5)))
  (check-true (string-contains? out "[...truncated]"))
) ;let

(check-report)
