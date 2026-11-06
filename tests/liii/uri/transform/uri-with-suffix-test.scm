;; uri-with-suffix
;; 替换或添加 URI 路径末级名称的文件后缀。
;;
;; 语法
;; ----
;; (uri-with-suffix uri-obj new-suffix)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要修改的 URI 对象。
;; new-suffix : string
;;   不含前导点的新文件后缀。
;;
;; 返回值
;; ----
;; uri?
;;   返回修改后缀后的新 URI；没有路径末级名称时返回原 URI。

(import (liii check) (liii uri record) (liii uri transform))

(check-set-mode! 'report-failed)

;; 替换已有后缀

(define u1 (make-uri-raw "https" "example.com" "/docs/manual.txt" '() #f))

(define u2 (uri-with-suffix u1 "html"))
(check (uri-path u2) => "/docs/manual.html")
(check (uri-path u1) => "/docs/manual.txt")

;; 多重后缀只替换最后一级

(define u3 (make-uri-raw "https" "example.com" "/archive.tar.gz" '() #f))
(check (uri-path (uri-with-suffix u3 "zip")) => "/archive.tar.zip")

;; 无后缀时添加后缀

(define u4 (make-uri-raw "https" "example.com" "/README" '() #f))
(check (uri-path (uri-with-suffix u4 "md")) => "/README.md")

;; 空路径保持不变

(define u5 (make-uri-raw "https" "example.com" "" '() #f))
(check (uri-path (uri-with-suffix u5 "txt")) => "")

;; 保留查询参数和片段

(define u6 (make-uri-raw "https" "example.com" "/a.txt" '(("k" . "v")) "frag"))

(define u7 (uri-with-suffix u6 "scm"))
(check (uri-query-ref u7 "k") => "v")
(check (uri-fragment u7) => "frag")

(check-report)
