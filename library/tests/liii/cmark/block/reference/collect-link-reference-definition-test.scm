;; collect-link-reference-definition
;; 从行列表开头尝试解析一条链接引用定义。
;;
;; 语法
;; ----
;; (collect-link-reference-definition lines)
;;
;; 参数
;; ----
;; lines : list
;; 行字符串列表。
;;
;; 返回值
;; ----
;; pair 或 #f
;; 成功返回 ((label url . title) . remaining-lines)，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block reference))

(check-set-mode! 'report-failed)

(let ((res (collect-link-reference-definition '("[id]: https://example.com \"Title\""
                                                "next")
           ) ;collect-link-reference-definition
      ) ;res
     ) ;
  (check (car res) => '("id" "https://example.com" . "Title"))
  (check (cdr res) => '("next"))
) ;let

(check-report)
