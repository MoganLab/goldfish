
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check) (liii cmark inline parser) (liii cmark node))

(check-set-mode! 'report-failed)

;; parse-bare-autolink
;; 解析不带尖括号的 http / https 裸 URL。
;;
;; 语法
;; ----
;; (parse-bare-autolink str start)

(check-true (bare-autolink-start? "https://example.com" 0))
(check-true (bare-autolink-start? "http://example.com/path" 0))
(check-false (bare-autolink-start? "abchttps://example.com" 3))
(check-false (bare-autolink-start? "www.example.com" 0))

(let ((result (parse-bare-autolink "https://example.com" 0)))
  (check (cmark-node-type (car result)) => 'link)
  (check
    (cdr (assoc 'url (cmark-node-data (car result))))
    =>
    "https://example.com"
  ) ;check
  (check (cdr result) => 19)
) ;let

(let* ((result (parse-bare-autolink "https://example.com." 0)) (node (car result)))
  (check (cmark-node-type node) => 'link)
  (check (cdr (assoc 'url (cmark-node-data node))) => "https://example.com")
  (check (cdr result) => 19)
) ;let*

(let* ((result (parse-bare-autolink "https://example.com/docs)." 0))
       (node (car result))
      ) ;
  (check (cmark-node-type node) => 'link)
  (check (cdr (assoc 'url (cmark-node-data node))) => "https://example.com/docs")
  (check (cdr result) => 24)
) ;let*

(let* ((result (parse-bare-autolink "https://example.com?a=1&amp;b=2" 0))
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
