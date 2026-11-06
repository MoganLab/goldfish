;; uri-with-name
;; 替换 URI 路径的最后一级名称，保留父路径。
;;
;; 语法
;; ----
;; (uri-with-name uri-obj new-name)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要修改的 URI 对象。
;; new-name : string
;;   新的路径末级名称。
;;
;; 返回值
;; ----
;; uri?
;;   返回替换路径末级名称后的新 URI。

(import (liii check) (liii uri record) (liii uri transform))

(check-set-mode! 'report-failed)

;; 替换多级路径中的文件名
(define u1 (make-uri-raw "https" "example.com" "/docs/old.txt" '() #f))
(define u2 (uri-with-name u1 "new.scm"))
(check (uri-path u2) => "/docs/new.scm")
(check (uri-path u1) => "/docs/old.txt")

;; 单级路径直接替换名称
(define u3 (make-uri-raw "https" "example.com" "old.txt" '() #f))
(check (uri-path (uri-with-name u3 "new.txt")) => "new.txt")

;; 根目录下的名称保留前导斜杠
(define root-file (make-uri-raw "https" "example.com" "/old.txt" '() #f))
(check (uri-path (uri-with-name root-file "new.txt")) => "/new.txt")

;; 目录路径将新名称追加到父路径
(define u4 (make-uri-raw "https" "example.com" "/docs/" '() #f))
(check (uri-path (uri-with-name u4 "index.html")) => "/docs/index.html")

;; 保留其他组件
(define u5 (make-uri-raw "https" "example.com" "/a/b" '(("k" . "v")) "frag"))
(define u6 (uri-with-name u5 "c"))
(check (uri-query-ref u6 "k") => "v")
(check (uri-fragment u6) => "frag")

(check-report)
