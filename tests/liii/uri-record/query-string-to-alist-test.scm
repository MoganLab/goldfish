(import (liii check)
        (liii uri-record)
) ;import

(check-set-mode! 'report-failed)

;; query-string->alist
;; 将查询字符串解析为 alist。
;;
;; 语法
;; ----
;; (query-string->alist qs)
;;
;; 返回值
;; ----
;; alist?
;;   返回键值对的关联列表。

;; 简单解析
(check (query-string->alist "a=1") => '(("a" . "1")))
(check (query-string->alist "a=1&b=2") => '(("a" . "1") ("b" . "2")))

;; 空值
(check (query-string->alist "key=") => '(("key" . "")))

;; 无等号（只有 key）
(check (query-string->alist "flag") => '(("flag" . "")))

;; URL 编码的值
(check (query-string->alist "name=hello%20world") => '(("name" . "hello world")))

;; 空字符串
(check (query-string->alist "") => '())
(check (query-string->alist #f) => '())

(check-report)
