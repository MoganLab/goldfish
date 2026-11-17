;; parse-autolink
;; 解析尖括号包裹的自动链接或邮箱地址。
;;
;; 语法
;; ----
;; (parse-autolink str start)
;;
;;
;; 参数
;; ----
;; str : any
;; 输入参数。
;; start : any
;; 输入参数。
;;
;; 返回值
;; ----
;; cmark-node 或 pair 或 #f
;; 返回解析得到的节点或结构。
;; 描述
;; ----
;; autolink-start? 用于判断当前位置是否可能是自动链接入口。

(import (liii check) (liii cmark inline parser) (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (autolink-start? "<https://example.com>" 0))
(check-true (autolink-start? "<user@example.com>" 0))
(check-false (autolink-start? "<not a link>" 0))

(let ((result (parse-autolink "<https://example.com>" 0)))
  (check (cmark-node-type (car result)) => 'link)
  (check
    (cdr (assoc 'url (cmark-node-data (car result))))
    =>
    "https://example.com"
  ) ;check
  (check (cdr result) => 21)
) ;let

(let ((result (parse-autolink "<user@example.com>" 0)))
  (check (cmark-node-type (car result)) => 'link)
  (check (cdr result) => 18)
) ;let

(let* ((result (parse-autolink "<https://example.com?a=1&amp;b=2>" 0))
       (node (car result))
      ) ;
  (check (cmark-node-type node) => 'link)
  (check (cdr (assoc 'url (cmark-node-data node)))
    =>
    "https://example.com?a=1&b=2"
  ) ;check
  (check
    (cdr
      (assoc 'content (cmark-node-data (car (cmark-node-children node))))
    ) ;cdr
    =>
    "https://example.com?a=1&b=2"
  ) ;check
) ;let*

(check-report)
