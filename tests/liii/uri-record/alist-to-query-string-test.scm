(import (liii check) (liii uri-record))


(check-set-mode! 'report-failed)


;; alist->query-string
;; 将 alist 转为查询字符串。
;;
;; 语法
;; ----
;; (alist->query-string alist)
;;
;; 返回值
;; ----
;; string?
;;   返回查询字符串。


;; 简单转换
(check (alist->query-string '(("a" . "1")))
  =>
  "a=1"
) ;check
(check (alist->query-string '(("a" . "1") ("b" . "2"))
       ) ;alist->query-string
  =>
  "a=1&b=2"
) ;check


;; 空值
(check (alist->query-string '(("key" . "")))
  =>
  "key="
) ;check


;; 只有 key（值为 #f）
(check (alist->query-string '(("flag" . #f)))
  =>
  "flag"
) ;check


;; URL 编码
(check (alist->query-string '(("name" . "hello world"))
       ) ;alist->query-string
  =>
  "name=hello%20world"
) ;check


;; 空列表
(check (alist->query-string '()) => "")


(check-report)
