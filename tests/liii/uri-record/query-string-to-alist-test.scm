(import (liii check) (liii uri-record))


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
(check (query-string->alist "a=1")
  =>
  '(("a" . "1"))
) ;check
(check (query-string->alist "a=1&b=2")
  =>
  '(("a" . "1") ("b" . "2"))
) ;check


;; 空值
(check (query-string->alist "key=")
  =>
  '(("key" . ""))
) ;check


;; 无等号（只有 key）
(check (query-string->alist "flag")
  =>
  '(("flag" . ""))
) ;check


;; URL 编码的值
(check (query-string->alist "name=hello%20world"
       ) ;query-string->alist
  =>
  '(("name" . "hello world"))
) ;check


;; 空字符串
(check (query-string->alist "") => '())
(check (query-string->alist #f) => '())


(check-report)
