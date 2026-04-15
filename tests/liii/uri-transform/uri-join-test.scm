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
(define base
  (string->uri "https://example.com/docs/api/"
  ) ;string->uri
) ;define
(define ref
  (string->uri "../guide/intro.md")
) ;define
(define joined (uri-join base ref))


(check (uri-path joined)
  =>
  "/docs/guide/intro.md"
) ;check
(check (uri->string joined)
  =>
  "https://example.com/docs/guide/intro.md"
) ;check


(check-report)
