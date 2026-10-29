(import (liii check) (liii uri))


(check-set-mode! 'report-failed)


;; uri-join
;; 合并两个 URI（RFC 3986）
;;
;; 语法
;; ----
;; (uri-join base-uri ref-uri)
;;
;; 返回值
;; ----
;; uri?
;;   返回新的 URI 记录。


;; 合并相对路径引用

(define base (string->uri "https://example.com/docs/api/"))

(define ref (string->uri "../guide/intro.md"))

(define joined (uri-join base ref))


(check (uri-path joined) => "/docs/guide/intro.md")
(check (uri->string joined) => "https://example.com/docs/guide/intro.md")


;; RFC 3986 5.2.3：合并相对路径引用时保留结尾斜杠

(check (uri->string (uri-join (string->uri "http://a/x/") (string->uri "p/")))
  => "http://a/x/p/"
)

(check (uri->string (uri-join (string->uri "http://a/x/") (string->uri "p/q/")))
  => "http://a/x/p/q/"
)

(check (uri->string (uri-join (string->uri "http://a/x/y") (string->uri "p/")))
  => "http://a/x/p/"
)


;; 连续合并时，目录型相对引用仍应作为后续引用的 base path

(define directory-uri
  (uri-join (string->uri "http://h/draft/") (string->uri "baseUriChange/"))
)


(check (uri->string directory-uri) => "http://h/draft/baseUriChange/")
(check (uri->string (uri-join directory-uri (string->uri "file.json")))
  => "http://h/draft/baseUriChange/file.json"
)


(check-report)
